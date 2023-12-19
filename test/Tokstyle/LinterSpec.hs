{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.LinterSpec
    ( mustParse
    , mustParseExpr
    , mustParseStmt
    , spec
    ) where

import           Test.Hspec         (Spec, it, shouldBe)

import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Language.Cimple    (Lexeme, Node)
import           Language.Cimple.IO (parseExpr, parseStmt, parseText)
import           Tokstyle.Linter    (allWarnings, analyseLocal)


mustParse :: MonadFail m => [Text] -> m [Node (Lexeme Text)]
mustParse code =
    case parseText $ Text.unlines code of
        Left err -> fail err
        Right ok -> return ok

mustParseExpr :: MonadFail m => [Text] -> m (Node (Lexeme Text))
mustParseExpr code =
    case parseExpr $ Text.unlines code of
        Left err -> fail err
        Right ok -> return ok

mustParseStmt :: MonadFail m => [Text] -> m (Node (Lexeme Text))
mustParseStmt code =
    case parseStmt $ Text.unlines code of
        Left err -> fail err
        Right ok -> return ok


spec :: Spec
spec = do
    it "should parse a simple function" $ do
        ast <- mustParse ["int a(void) { return 3; }"]
        analyseLocal allWarnings ("test.c", ast) `shouldBe` []

    it "should give diagnostics on extern decls in .c files" $ do
        ast <- mustParse ["int a(void);"]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe` ["test.c:1: global function `a` declared in .c file [-Wglobal-funcs]"]

    it "should not give diagnostics on extern decls in .h files" $ do
        ast <- mustParse ["int a(void);"]
        analyseLocal allWarnings ("test.h", ast) `shouldBe` []
