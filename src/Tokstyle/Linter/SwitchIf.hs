{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.SwitchIf (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.List                   (nub)
import           Data.Text                   (Text)
import           Language.Cimple             (BinaryOp (..), Lexeme (..),
                                              LiteralType (..), Node,
                                              NodeF (..), lexemeText)
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)


pattern EqualsConst :: Lexeme Text -> Node (Lexeme Text)
pattern EqualsConst lhs <- Fix (BinaryExpr (Fix (VarExpr lhs)) BopEq (Fix (LiteralExpr ConstId _)))


data IfInfo = IfInfo
    { ifConds    :: Maybe [Lexeme Text]
    , ifBranches :: [Node (Lexeme Text)]
    } deriving (Show)

instance Semigroup IfInfo where
    a <> b = IfInfo ((<>) <$> ifConds a <*> ifConds b) (ifBranches a <> ifBranches b)


collectInfo :: Node (Lexeme Text) -> IfInfo
collectInfo (Fix (IfStmt (EqualsConst lhs) t Nothing)) =
    IfInfo (Just [lhs]) [t]
collectInfo (Fix (IfStmt (EqualsConst lhs) t (Just e))) =
    IfInfo (Just [lhs]) [t] <> collectInfo e
collectInfo (Fix (IfStmt _ t Nothing)) =
    IfInfo Nothing [t]
collectInfo (Fix (IfStmt _ t (Just e))) =
    IfInfo Nothing [t] <> collectInfo e
collectInfo e =
    IfInfo (Just []) [e]


-- | Returns 'True' if there are at least 3 if conditions comparing a variable
-- to a constant and all variable names are the same. Additionally checks
-- whether all branches are single statements, in which case it returns
-- 'False'.
shouldDiagnose :: [Lexeme Text] -> [Node (Lexeme Text)] -> Bool
shouldDiagnose cs branches =
    length cs >= 3 && length (nub $ map lexemeText cs) == 1 && not (all singleStatement branches)
  where
    singleStatement (Fix (CompoundStmt [_])) = True
    singleStatement _                        = False


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            IfStmt{} -> do
                let info = collectInfo node
                case ifConds info of
                  Just cs@(c:_) | shouldDiagnose cs (ifBranches info) ->
                      warn file c "if-statement could be a switch"
                  _ -> return ()
                traverseAst linter (file, ifBranches info)

            _ -> act
    }


analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse = reverse . flip State.execState [] . traverseAst linter
