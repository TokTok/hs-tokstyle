{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.TypeCheckSpec where

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
        it "should not crash on input" $ do
            ast <- mustParse
                [ "int a(void) {"
                , "  int i;"
                , "  put_int(i);"
                , "}"
                ]
            analyse ["type-check"] ("test.c", ast)
                `shouldBe`
                []
