{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.BoolReturnSpec where

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
        it "should give diagnostics on simplifiable if-return" $ do
            ast <- mustParse
                [ "int a(int b) {"
                , "  if (b == 0) { return true; }"
                , "  return false;"
                , "}"
                ]
            analyse allWarnings ("test.c", ast)
                `shouldBe`
                [ "test.c:2: if-statement followed by boolean return can be simplified to return [-Wbool-return]"
                ]

        it "should give diagnostics on simplifiable if/else with boolean return" $ do
            ast <- mustParse
                [ "int a(int b) {"
                , "  if (b == 0) { return true; }"
                , "  else { return false; }"
                , "}"
                ]
            analyse allWarnings ("test.c", ast)
                `shouldBe`
                [ "test.c:2: if/else with return true/false can be simplified to return [-Wbool-return]"
                ]

        it "should give diagnostics when there are other statements before the violating code" $ do
            ast <- mustParse
                [ "int a(int b) {"
                , "  do_something();"
                , "  if (b == 0) { return true; }"
                , "  return false;"
                , "}"
                ]
            analyse allWarnings ("test.c", ast)
                `shouldBe`
                [ "test.c:3: if-statement followed by boolean return can be simplified to return [-Wbool-return]"
                ]
