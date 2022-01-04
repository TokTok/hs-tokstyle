{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.VarUnusedInScope where

import           Control.Monad               (foldM)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..), foldFixM)
import           Data.List                   (delete, find)
import           Data.Text                   (Text)
import           Language.Cimple             (AstActions', Lexeme (..), Node,
                                              NodeF (..), UnaryOp (..),
                                              defaultActions', doNode,
                                              lexemeText, traverseAst)
import           Language.Cimple.Diagnostics (Diagnostics', warn')


data ReadKind
    = Alone
    | Nested
    deriving (Show, Eq)

data Action
    = Declare
    | Write
    | Read ReadKind
    | WriteThenRead
    deriving (Show, Eq)

data Var = Var Action (Lexeme Text)
  deriving (Show, Eq)

lookupVar :: Var -> [Var] -> Maybe Var
lookupVar = find . sameName
  where
    sameName (Var _ (L _ _ n1)) (Var _ (L _ _ n2)) = n1 == n2

combine :: [Var] -> [Var] -> [Var]
combine = foldr $ \var2@(Var act2 _) ls ->
    case lookupVar var2 ls of
        Nothing                          -> var2 : ls
        Just (Var act1 _) | act1 == act2 -> ls
        Just var1                        -> var2 : delete var1 ls

combineBranches :: [Var] -> [Var] -> [Var]
combineBranches ls1 ls2 = foldr join [] (ls1 ++ ls2)
  where
    join :: Var -> [Var] -> [Var]
    join var1 ls =
        case lookupVar var1 ls of
            Nothing   -> var1 : ls
            Just var2 ->
                let joined = joinVar var1 var2 in
                joined ++ delete var2 ls

    joinVar var1@(Var act1 _) (Var act2 _) | act1 == act2 = [var1]
    joinVar var1@(Var Read{} _) (Var Read{} _) = [var1]

    joinVar var1@(Var Read{} _) (Var Write _) = [var1]
    joinVar (Var Write _) var2@(Var Read{} _) = [var2]

    joinVar var1 var2 = error ("combineBranches" <> show (var1, var2))

combineStatements :: FilePath -> [Var] -> [Var] -> Diagnostics' [Var]
combineStatements file ls1 ls2 = foldM join [] (ls1 ++ ls2)
  where
    join :: [Var] -> Var -> Diagnostics' [Var]
    join ls var1 =
        case lookupVar var1 ls of
            Nothing   -> return $ var1 : ls
            Just var2 -> do
                joined <- joinVar var1 var2
                return $ joined ++ delete var2 ls

    joinVar var1@(Var Read{} _) (Var Read{} _)        = return [var1]
    joinVar var1@(Var Read{} _) (Var Write _)         = return [var1]
    joinVar var1@(Var Read{} _) (Var WriteThenRead _) = return [var1]
    joinVar var1@(Var Write _)  (Var Write _)         = return [var1]
    joinVar (Var Write l1) (Var Read{} _)             = return [Var WriteThenRead l1]
    joinVar (Var Write l1) (Var WriteThenRead _)      = return [Var WriteThenRead l1]
    joinVar var1@(Var WriteThenRead _) _              = return [var1]

    joinVar (Var Declare l1) (Var Write l2) = do
        warn' file l1 $ "variable `" <> lexemeText l1 <> "' can be reduced in scope"
        warn' file l2 "  possibly to here"
        return []

    joinVar (Var Declare _) _ = return []
    joinVar var1@(Var _ l1) (Var Declare l2) = do
        warn' file l1 $ "variable `" <> lexemeText l1 <> "' used before its declaration"
        warn' file l2 $ "  `" <> lexemeText l2 <> "' was declared here"
        return [var1]


checkScopes :: FilePath -> NodeF (Lexeme Text) [Var] -> Diagnostics' [Var]
checkScopes file = \case
    CompoundStmt ls             -> checkCompoundStmt ls
    ForStmt decl cont incr body -> return $ foldr combine [] [body, incr, cont, decl]
    IfStmt c t Nothing          -> return $ t ++ c
    IfStmt c t (Just e)         -> return $ combineBranches t e ++ c

    DeclSpecVar var             -> return [Var Declare var]

    VarExpr var                 -> return [Var (Read Alone) var]
    AssignExpr lhs _ []         -> return $ map readToWrite lhs
    BinaryExpr lhs _ rhs        -> return $ combine lhs rhs
    UnaryExpr UopIncr expr      -> return $ map writeToRead expr
    UnaryExpr UopDecr expr      -> return $ map writeToRead expr
    FunctionCall f args         -> return $ foldr combine [] (f : args)

    UnaryExpr UopDeref e        -> return $ map readToNested e
    MemberAccess e _            -> return $ map readToNested e
    PointerAccess e _           -> return $ map readToNested e
    ArrayAccess e i             -> return $ map readToNested (e ++ i)

    PreprocIf _ t e             -> checkCompoundStmt $ e : t
    PreprocIfdef _ t e          -> checkCompoundStmt $ e : t
    PreprocIfndef _ t e         -> checkCompoundStmt $ e : t
    PreprocElse e               -> checkCompoundStmt e

    n                           -> return $ foldr combine [] n

  where
    checkCompoundStmt =
        fmap (map wtrToWrite) . foldM (combineStatements file) [] . reverse

    wtrToWrite (Var WriteThenRead var) = Var Write var
    wtrToWrite var                     = var

    readToNested (Var (Read Alone) var) = Var (Read Nested) var
    readToNested var                    = var

    readToWrite (Var (Read Alone) var) = Var Write var
    readToWrite var                    = var

    writeToRead (Var Write var)         = Var (Read Alone) var
    writeToRead (Var WriteThenRead var) = Var (Read Alone) var
    writeToRead var                     = var


linter :: AstActions' [Text]
linter = defaultActions'
    { doNode = \file node act ->
        case node of
            Fix FunctionDefn{} -> do
                _ <- foldFixM (checkScopes file) node
                return node

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
