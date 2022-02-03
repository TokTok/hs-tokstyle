{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}
module Tokstyle.Linter.LoggerConst (analyse) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import           Language.Cimple             ( Lexeme (..),
                                              Node, NodeF (..))
import           Language.Cimple.TraverseAst             (AstActions,
                                              astActions,
                                              doNode, traverseAst)
import           Language.Cimple.Diagnostics (warn)
import           System.FilePath             (takeFileName)


linter :: AstActions (State [Text]) Text
linter = astActions
    { doNode = \file node act ->
        case unFix node of
            -- Ignore struct members and variable declarations. In structs, we
            -- may store a non-const Logger so we can later free it. In variable
            -- declarations, we may be creating a new Logger.
            Struct{} -> return ()
            VarDeclStmt{} -> return ()

            VarDecl (Fix (TyPointer (Fix (TyUserDefined (L _ _ "Logger"))))) name _ -> do
                warn file name $ "Logger parameter should be pointer-to-const"
                act

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse (file, _) | takeFileName file `elem` ["logger.c", "logger.h"] = []
analyse tu = reverse . flip State.execState [] . traverseAst linter $ tu
