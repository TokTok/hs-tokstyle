{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.VarUnusedInScope where

import           Control.Monad               (foldM)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..), foldFixM)
import           Data.List                   (delete)
import           Data.Maybe                  (listToMaybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Debug.Trace                 (trace)
import           Language.Cimple             (AstActions', Lexeme (..), Node,
                                              NodeF (..), UnaryOp (..),
                                              defaultActions', doNode,
                                              lexemeText, traverseAst)
import           Language.Cimple.Diagnostics (Diagnostics', warn')


data Action
    = Declare
    | Write
    | Read
    | ReadOnly
    | WriteThenRead
    deriving (Show, Eq)

data Var = Var Action (Lexeme Text)
  deriving (Show, Eq)

sameName :: Var -> Var -> Bool
sameName (Var _ (L _ _ n1)) (Var _ (L _ _ n2)) = n1 == n2

lookupVar :: Var -> [Var] -> Maybe Var
lookupVar v = listToMaybe . filter (sameName v)

combine :: [Var] -> [Var] -> [Var]
combine ls1 ls2 = foldr join ls1 ls2
  where
    join :: Var -> [Var] -> [Var]
    join var2@(Var act2 _) l1 =
        case lookupVar var2 l1 of
            Nothing                          -> var2 : l1
            Just (Var act1 _) | act1 == act2 -> l1
            Just var1                        -> var2 : delete var1 l1

combineBranches :: [Var] -> [Var] -> [Var]
combineBranches ls1 ls2 = foldr join [] (ls1 ++ ls2)
  where
    join :: Var -> [Var] -> [Var]
    join var1 ls = do
        case lookupVar var1 ls of
            Nothing   -> var1 : ls
            Just var2 ->
                let joined = joinVar var1 var2 in
                joined ++ delete var2 ls

    joinVar var1@(Var act1 _) (Var act2 _) | act1 == act2 = [var1]

    joinVar var1@(Var Read _) (Var ReadOnly _) = [var1]
    joinVar (Var ReadOnly _) var2@(Var Read _) = [var2]

    joinVar var1@(Var Read _) (Var Write _) = [var1]
    joinVar var1@(Var ReadOnly _) (Var Write _) = [var1]
    joinVar (Var Write _) var2@(Var Read _) = [var2]
    joinVar (Var Write _) var2@(Var ReadOnly _) = [var2]
    joinVar var1 var2 = error (show (var1, var2))

compound :: FilePath -> Lexeme Text -> [Var] -> [Var] -> Diagnostics' [Var]
compound file name ls1 ls2 = foldM join [] (ls1 ++ ls2)
  where
    join :: [Var] -> Var -> Diagnostics' [Var]
    join [] var1 = return [var1]
    join ls var1 = do
        case lookupVar var1 ls of
            Nothing   -> return $ var1 : ls
            Just var2 -> do
                joined <- joinVar var1 var2
                return $ joined ++ delete var2 ls

    joinVar var1@(Var Read _) (Var Read _) = return [var1]
    joinVar var1@(Var ReadOnly _) (Var ReadOnly _) = return [var1]
    joinVar var1@(Var Read _) (Var ReadOnly _) = return [var1]
    joinVar (Var ReadOnly _) var2@(Var Read _) = return [var2]
    joinVar var1@(Var Write _) (Var Write _) = return [var1]
    joinVar var1@(Var Read _) (Var Write _) = return [var1]
    joinVar var1@(Var ReadOnly _) (Var Write _) = return [var1]
    joinVar var1@(Var Read _) (Var WriteThenRead _) = return [var1]
    joinVar var1@(Var ReadOnly _) (Var WriteThenRead _) = return [var1]
    joinVar var1@(Var WriteThenRead _) _ = return [var1]

    joinVar (Var Write l1) (Var WriteThenRead _) = return [Var WriteThenRead l1]
    joinVar (Var Declare _) (Var Declare _) = return []
    joinVar (Var Write l1) (Var Read _) = return [Var WriteThenRead l1]
    joinVar (Var Write l1) (Var ReadOnly _) = return [Var WriteThenRead l1]
    joinVar (Var Declare _) (Var Read _) = return []
    joinVar (Var Declare _) (Var ReadOnly _) = return []
    joinVar (Var Declare _) (Var WriteThenRead _) = return []

    joinVar (Var Declare l1) (Var Write l2) = do
        warn' file l1 $ "variable `" <> lexemeText l1 <> "' can be reduced in scope"
        warn' file l2 $ "  possibly to here"
        return []
    joinVar var1 var2 = do
        warn' file name $ Text.pack (show (var1, var2))
        return []


checkScopes :: FilePath -> Lexeme Text -> NodeF (Lexeme Text) [Var] -> Diagnostics' [Var]
checkScopes file name = \case
    TyStd{}                     -> return []
    TyUserDefined{}             -> return []
    FunctionPrototype ty _ args -> return $ foldr combine ty args
    FunctionDefn _ args body    -> return $ combine args body
    LiteralExpr{}               -> return []
    Return Nothing              -> return []
    Return (Just ls)            -> return ls
    CompoundStmt ls             -> checkCompound ls
    ForStmt decl cont incr body -> return $ foldr combine [] [body, incr, cont, decl]
    IfStmt c t Nothing          -> return $ t ++ c
    IfStmt c t (Just e)         -> return $ combineBranches t e ++ c

    DeclSpecVar var             -> return $ [Var Declare var]
    Declarator var Nothing      -> return $ var
    Declarator var (Just ini)   -> return $ var ++ ini
    VarDecl _ decl              -> return $ decl

    VarExpr var                 -> return $ [Var Read var]
    AssignExpr lhs _ []         -> return $ map readToWrite lhs
    BinaryExpr lhs _ rhs        -> return $ combine lhs rhs
    UnaryExpr UopIncr expr      -> return $ map writeToRead expr
    UnaryExpr UopDecr expr      -> return $ map writeToRead expr
    FunctionCall f args         -> return $ foldr combine [] (f : args)

    UnaryExpr UopDeref e        -> return $ map readToReadOnly e
    MemberAccess e _            -> return $ map readToReadOnly e
    PointerAccess e _           -> return $ map readToReadOnly e
    ArrayAccess e i             -> return $ map readToReadOnly (e ++ i)

    PreprocIf _ t e             -> checkCompound $ e : t
    PreprocIfdef _ t e          -> checkCompound $ e : t
    PreprocIfndef _ t e         -> checkCompound $ e : t
    PreprocElse e               -> checkCompound e

    n                           -> return $ foldr combine [] n

  where
    checkCompound = fmap (map wtrToWrite) . foldM (compound file name) [] . reverse

    wtrToWrite (Var WriteThenRead var) = Var Write var
    wtrToWrite var                     = var

    readToReadOnly (Var Read var) = Var ReadOnly var
    readToReadOnly var            = var

    readToWrite (Var Read var) = Var Write var
    readToWrite var            = var

    writeToRead (Var Write var)         = Var Read var
    writeToRead (Var WriteThenRead var) = Var Read var
    writeToRead var                     = var


linter :: AstActions' [Text]
linter = defaultActions'
    { doNode = \file node act ->
        case node of
            Fix (FunctionDefn _ (Fix (FunctionPrototype _ name _)) _) -> do
                _ <- foldFixM (checkScopes file name) node
                return $ trace (show name) node

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
