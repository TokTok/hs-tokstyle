{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.LinterSpec where

import           Test.Hspec         (Spec, describe, it, shouldBe)

import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Language.Cimple    (Lexeme, Node)
import           Language.Cimple.IO (parseText)
import           Tokstyle.Linter    (allWarnings, analyse)


mustParse :: MonadFail m => [Text] -> m [Node (Lexeme Text)]
mustParse code =
    case parseText $ Text.unlines code of
        Left err -> fail err
        Right ok -> return ok


spec :: Spec
spec =
    describe "analyse" $ do
        it "should parse a simple function" $ do
            let Right ast = parseText "int a(void) { return 3; }"
            analyse allWarnings ("test.c", ast) `shouldBe` []

        it "should give diagnostics on extern decls in .c files" $ do
            let Right ast = parseText "int a(void);"
            analyse allWarnings ("test.c", ast)
                `shouldBe` ["test.c:1: global function `a` declared in .c file [-Wglobal-funcs]"]

        it "should not give diagnostics on extern decls in .h files" $ do
            let Right ast = parseText "int a(void);"
            analyse allWarnings ("test.h", ast) `shouldBe` []
