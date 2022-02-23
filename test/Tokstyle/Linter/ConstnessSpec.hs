{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.ConstnessSpec where

import           Test.Hspec         (Spec, describe, it, shouldBe)

import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Language.Cimple    (Lexeme, Node)
import           Language.Cimple.IO (parseText)
import           Tokstyle.Linter    (analyse)


mustParse :: MonadFail m => [Text] -> m [Node (Lexeme Text)]
mustParse code =
    case parseText $ Text.unlines code of
        Left err -> fail err
        Right ok -> return ok


spec :: Spec
spec =
    describe "analyse" $ do
        it "should not give diagnostics on parameters or pointer or array types" $ do
            ast <- mustParse
                [ "int a(int b) {"
                , "  int *a = get();"
                , "  int b[3];"
                , "  return *a + b[0];"
                , "}"
                ]
            analyse ["constness"] ("test.c", ast)
                `shouldBe` []

        it "should give diagnostics on locals that can be const" $ do
            ast <- mustParse
                [ "int a(int b) {"
                , "  int a = get();"
                , "  return a;"
                , "}"
                ]
            analyse ["constness"] ("test.c", ast)
                `shouldBe`
                [ "test.c:2: variable `a` is never written to and can be declared `const` [-Wconstness]"
                ]
