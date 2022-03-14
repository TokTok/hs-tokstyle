{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CallocTypeSpec where

import           Test.Hspec         (Spec, it, shouldBe)

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
spec = do
    it "detects when calloc() is used with built-in types" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  uint8_t *a = (uint8_t *)calloc(1, sizeof(uint8_t));"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            ["test.c:2: `calloc` should not be used for `\ESC[32muint8_t\ESC[0m*`; use `malloc` instead [-Wcalloc-type]"]

    it "detects when calloc() result is cast to the wrong type" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  uint8_t *a = (uint8_t *)calloc(1);"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: invalid `calloc` invocation: 2 arguments expected [-Wcalloc-args]"
            , "test.c:2: the result of `calloc` must be cast to its member type [-Wcalloc-type]"
            ]

    it "detects when calloc() result is not cast to any type" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  void *a = calloc(1, sizeof(int));"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe`
            ["test.c:2: the result of `calloc` must be cast to its member type [-Wcalloc-type]"]

    it "should not give diagnostics calloc() used correctly" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  Foo *a = (Foo *)calloc(1, sizeof(Foo));"
            , "}"
            ]
        analyse allWarnings ("test.c", ast)
            `shouldBe` []
