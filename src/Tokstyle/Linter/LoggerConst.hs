{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
module Tokstyle.Linter.LoggerConst (descr) where

import           Control.Monad.State.Strict  (State)
import qualified Control.Monad.State.Strict  as State
import           Data.Fix                    (Fix (..))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           Language.Cimple             (Lexeme (..), Node, NodeF (..))
import           Language.Cimple.Diagnostics (warn)
import           Language.Cimple.TraverseAst (AstActions, astActions, doNode,
                                              traverseAst)
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

            VarDecl (Fix (TyPointer (Fix (TyUserDefined (L _ _ "Logger"))))) name _ ->
                warn file name "Logger parameter should be pointer-to-const"

            _ -> act
    }

analyse :: (FilePath, [Node (Lexeme Text)]) -> [Text]
analyse (file, _) | takeFileName file `elem` ["logger.c", "logger.h"] = []
analyse tu = reverse . flip State.execState [] . traverseAst linter $ tu

descr :: ((FilePath, [Node (Lexeme Text)]) -> [Text], (Text, Text))
descr = (analyse, ("logger-const", Text.unlines
    [ "Checks that `Logger` is always passed as `const Logger *` except in `logger.c`."
    , ""
    , "**Reason:** no functions should be modifying the logger after creation. We can"
    , "store a non-const logger in a struct so we can later pass it to `logger_free`,"
    , "but passing it down to other functions must never mutate the logger."
    ]))
