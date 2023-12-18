{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}
module Tokstyle.Linter.VarUnusedInScope (analyse) where

import           Control.Applicative         ((<|>))
import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..), foldFix)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Text                   (Text)
--import           Debug.Trace                 (trace)
import           Language.Cimple             (AssignOp (..), Lexeme (..), Node,
                                              NodeF (..), UnaryOp (..),
                                              lexemeText)
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
import           Lens.Micro                  (over, set, (^.))
import           Lens.Micro.TH               (makeLenses, makeLensesFor)
import           Text.Groom                  (groom)


data Action
    = NoReduce
    | Reduce
    | Declare
    | Read
    | Write
    | ReadWrite
    deriving (Show, Eq, Ord)

data Flags = Flags
    { _flgIndirect    :: Bool
      -- ^ A variable read within a more complex expression. All variable references start out with
      -- "potential write". On the LHS of an assignment, all direct variable references are turned
      -- into Write operations. Indirect means, the variable reference was part of an expression
      -- that makes it a pure read, so doesn't turn it into a write. Example: `*a = 3`, where `a` is
      -- not assigned.
    , _flgConditional :: Bool
    , _flgLoop        :: Bool
    , _flgNonConst    :: Bool
      -- ^ Whether an expression contains any non-constant expressions.
    , _flgNested      :: Int
    }
    deriving (Show, Eq)

makeLenses ''Flags

andFlags :: Flags -> Flags -> Flags
a `andFlags` b = Flags
    (a ^. flgIndirect || b ^. flgIndirect)
    -- Definite action followed by a conditional action is a definite action.
    -- Conditional action followed by a definite action is a conditional action.
    (a ^. flgConditional)
    -- Two loops in sequence mean overall these two loops are no longer part of an outer loop.
    False
    -- If any of the actions have side-effects, the combination does, too.
    (a ^. flgNonConst || b ^. flgNonConst)
    -- Two operations in sequence stop being nested.
    (min 0 (min (a ^. flgNested) (b ^. flgNested)))

orFlags :: Flags -> Flags -> Flags
a `orFlags` b = Flags
    (a ^. flgIndirect    || b ^. flgIndirect   )
    (a ^. flgConditional || b ^. flgConditional)
    (a ^. flgLoop        || b ^. flgLoop       )
    (a ^. flgNonConst    || b ^. flgNonConst   )
    -- Nested operations remain nested in conditionals if both were nested
    -- If one operation was not nested, e.g. one was part of the if-condition, the result is not
    -- nested anymore.
    (min (a ^. flgNested) (b ^. flgNested))

defaultFlags :: Flags
defaultFlags = Flags False False False False 0

data Operation = Operation
    { _opAct   :: Action
    , _opFlags :: Flags
    }
    deriving (Show, Eq)

makeLensesFor [("_opFlags", "opFlags")] ''Operation

op :: Action -> Operation
op = flip Operation defaultFlags

data Var = Var
    { _varOp   :: Operation
    , _varDecl :: Maybe (Lexeme Text)
    , _varUse  :: Maybe (Lexeme Text)
    }
    deriving (Show, Eq)

makeLensesFor [("_varOp", "varOp")] ''Var

-- | Turn direct reads on the lhs of an assignment into writes.
--
-- `++` and `--` are considered assignments.
readTo :: Action -> Var -> Var
readTo act (Var (Operation Read flg@Flags{_flgIndirect = False}) decl use) =
    Var (Operation act flg) decl use
readTo _ var = var

markConditional :: Map k Var -> Map k Var
markConditional = Map.map $ set (varOp.opFlags.flgConditional) True

-- | Combine 2 elements of a sequence of operations.
--
-- 'combineAnd a b' is called when first 'a' and then 'b' happens.
-- Example: `a = 3; print(a);` is a 'Write' and then a 'Read'.
combineAnd :: Var -> Var -> Var
-- Nested 'NoReduce' is thrown away when there's another variable with the same name at a higher up
-- scope.
combineAnd (Var (Operation NoReduce flg) _ _) v2 | flg^.flgNested > 0 = v2
combineAnd v1 (Var (Operation NoReduce flg) _ _) | flg^.flgNested > 0 = v1
-- All other situations are handled by the combineOp function below.
combineAnd (Var op1 decl1 use1) (Var op2 decl2 use2) =
--  trace (groom (Var op1 decl1 use1, "AND", Var op2 decl2 use2, "EQUALS", result) <> "\n") $
    result
  where
    result = Var (combineOp op1 op2) (decl1 <|> decl2) (use1 <|> use2)

    combineOp (Operation Declare a) (Operation ReadWrite b)
        -- Read and write happening in a loop => possibly can't reduce.
      | b^.flgLoop = Operation NoReduce a

    combineOp (Operation Declare a) (Operation _ b)
        -- Declaration has side-effects in its initialiser => keep it where it was.
      | a^.flgNonConst = Operation NoReduce a
        -- Declaration is in the same scope as an operation => ignore the declare, marking this
        -- particular declaration as not reducible. We may still find reducible ones later in case
        -- of shadowing or `#if`/`#else`.
      | b^.flgNested <= 0 = Operation NoReduce a
        -- Loops are nested twice: once for a potential for-init-decl and once for the body. The
        -- for-init-decl itself is not in the loop, but the other 2 components of the `for (...)`
        -- are. Anything declared in the current scope and referenced there will be at nesting level
        -- 1 and cannot be reduced in scope.
      | b^.flgLoop && b^.flgNested <= 1 = Operation NoReduce a
        -- The first thing happening to this variable is something conditional. We err on the side
        -- of safety and ignore this situation.
      | b^.flgLoop && b^.flgConditional = Operation NoReduce a
        -- Declaration has a trivial initialiser (no possible side-effects) and the read is nested,
        -- so we can reduce the scope of this variable.
      | not (a^.flgNonConst) && b^.flgNested > 0 = Operation Reduce a

    -- We found a declaration that can be reduced in scope and then an operation (maybe another
    -- declaration) on a variable with the same name but in a different scope. We ignore the second
    -- one and just keep the first declaration for diagnostics.
    combineOp (Operation Reduce a) _ | a^.flgNested > 0 = op1

    -- We already know the scope can't be reduced, so any combination in the same scope won't make a
    -- difference.
    combineOp (Operation NoReduce a) _ | a^.flgNested <= 0 = op1

    combineOp (Operation act1 flg1) (Operation act2 flg2) =
        Operation (combineAction act1 act2) (flg1 `andFlags` flg2)

    combineAction Reduce    Reduce    = Reduce
    combineAction Reduce    NoReduce  = Reduce
    combineAction NoReduce  NoReduce  = NoReduce

    -- We can reduce the scope of first one, and then we can copy the declaration for the second
    -- one. Example: `int i; for (i = 0; ...){} for (i = 0; ...){}`.
    combineAction Reduce    Write     = Reduce
    combineAction Reduce    ReadWrite = Reduce

    -- We thought we can reduce the scope, but then the result is actually read in the current
    -- scope, so we flip the decision to 'NoReduce'.
    combineAction Reduce    Read      = NoReduce

    -- Write, then read => keep the (possibly conditional) write.
    combineAction Write     Read      = Write
    combineAction Write     Write     = Write
    combineAction Write     ReadWrite = Write

    -- Two reads combine into a read.
    combineAction Read      Read      = Read

    -- Read or read/write then write: record as read/write.
    combineAction Read      Write     = ReadWrite
    combineAction ReadWrite Write     = ReadWrite
    combineAction ReadWrite Read      = ReadWrite
    combineAction Read      ReadWrite = ReadWrite
    combineAction ReadWrite ReadWrite = ReadWrite
    combineAction _ _                 = error (groom (op1, op2))

-- | Combine 2 elements of a choice of operations.
--
-- 'combineOr a b' is called when either 'a' or 'b' happens, but never both.
-- Example: `if (c) { a = 3; } else { print(a); }` is either a 'Write' or a 'Read'.
combineOr :: Var -> Var -> Var
combineOr v1@(Var (Operation Reduce _) _ _) (Var (Operation Reduce _) _ _) = v1
combineOr v1@(Var (Operation NoReduce _) _ _) (Var (Operation NoReduce _) _ _) = v1
combineOr v1@(Var (Operation Declare _) _ _) (Var (Operation Declare _) _ _) = v1

combineOr (Var (Operation act1 flg1) decl1 use1) (Var (Operation act2 flg2) decl2 use2) =
--  trace (groom ((Var (Operation act1 flg1) decl1 use1), "OR", (Var (Operation act2 flg2) decl2 use2), "EQUALS", result) <> "\n") $
    result
  where
    result = Var combinedOp (select decl1 decl2) (select use1 use2)

    combinedOp
        -- Definite read, write, or read+write: both branches do the same => remove conditional flag.
      | act1 == act2 = Operation act1 (flg1 `orFlags` flg2){_flgConditional = False}
        -- Either written or read => consider it as conditional write.
      | otherwise = Operation (max act1 act2) (flg1 `orFlags` flg2)

    select a@Just{} b@Just{} = if act1 > act2 then a else b
    select a b               = a <|> b

-- | Mark the callee as introducing side-effects into the expression.
processFunctionCall :: Map Text Var -> [Map Text Var] -> Map Text Var
processFunctionCall callee args =
    Map.unionsWith combineAnd $ Map.map (set (varOp.opFlags.flgNonConst) True) callee : args

varScopes :: NodeF (Lexeme Text) (Map Text Var) -> Map Text Var
varScopes = \case
    IfStmt c t Nothing       -> Map.unionWith combineAnd c $ markConditional t
    IfStmt c t (Just e)      -> Map.unionWith combineAnd c $ Map.unionWith combineOr (markConditional t) (markConditional e)

    -- Only non-array variables are recorded. Arrays are tricky because of implicit pointer
    -- conversion and the way we use them. See the "ignores array-typed variables" test cases for
    -- examples that are non-trivial to statically analyse with the method we're using here.
    VarDecl t var []         -> Map.insert (lexemeText var) (Var (op Declare) (Just var) Nothing) t
    VarDeclStmt var iexpr    -> maybe var (\e -> Map.unionWith combineAnd (propagateNonConst e var) e) iexpr

    VarExpr var              -> Map.singleton (lexemeText var) (Var (op Read) Nothing (Just var))
    AssignExpr lhs AopEq rhs -> Map.unionWith combineAnd (Map.map (readTo Write) lhs) rhs
    AssignExpr lhs _ rhs     -> Map.unionWith combineAnd (Map.map (readTo ReadWrite) lhs) rhs

    -- ++ and -- do both read and write.
    UnaryExpr UopIncr e      -> Map.map (readTo ReadWrite) e
    UnaryExpr UopDecr e      -> Map.map (readTo ReadWrite) e

    -- &var is considered a write, but since it can occur inside an expression (e.g. function call),
    -- it's considered negatively nested. When nesting once (e.g. inside a for-init-decl), it's not
    -- considered nested. When nesting twice (e.g. inside the for-body), it is considered nested
    -- once.
    UnaryExpr UopAddress e   -> nested (-1) $ Map.map (readTo Write) e
    UnaryExpr UopDeref e     -> indirect e
    MemberAccess e _         -> indirect e
    PointerAccess e _        -> indirect e
    ArrayAccess e i          -> indirect $ Map.unionWith combineAnd e i

    -- Ignore parameter declarations. These can never be reduced in scope.
    FunctionPrototype{}      -> Map.empty

    FunctionCall callee args -> processFunctionCall callee args

    ForStmt i c n b          -> nested 1 . Map.unionWith combineAnd i . inLoop . Map.unionsWith combineAnd $ [c, n, b]
    node@WhileStmt{}         -> nested 1 . inLoop . Map.unionsWith combineAnd $ node
    node@DoWhileStmt{}       -> nested 1 . inLoop . Map.unionsWith combineAnd $ node
    node@CompoundStmt{}      -> nested 1 . Map.unionsWith combineAnd $ node

    node                     -> Map.unionsWith combineAnd node
  where
    propagateNonConst e = if Map.null e then id else Map.map (set (varOp.opFlags.flgNonConst) True)
    indirect = Map.map (set (varOp.opFlags.flgIndirect) True)
    nested n = Map.map (over (varOp.opFlags.flgNested) (+n))
    inLoop = Map.map (set (varOp.opFlags.flgLoop) True)


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            FunctionDefn{} ->
                mapM_ (warnAbout file) . Map.elems . foldFix varScopes $ node

            _ -> act
    }
  where
      warnAbout file (Var (Operation Reduce _) (Just decl) (Just use)) = do
          warn file decl $ "variable `" <> lexemeText decl <> "` can be reduced in scope"
          warn file use    "  possibly to here"
      warnAbout _ _ = return ()

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
