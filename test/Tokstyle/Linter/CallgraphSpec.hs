{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.CallgraphSpec where

import           Test.Hspec         (Spec, describe, it, shouldBe)

import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Language.Cimple    (Lexeme, Node)
import           Language.Cimple.IO (parseText)
import           Tokstyle.Linter    (analyseGlobal)


mustParse :: MonadFail m => [Text] -> m [Node (Lexeme Text)]
mustParse code =
    case parseText $ Text.unlines code of
        Left err -> fail err
        Right ok -> return ok


spec :: Spec
spec =
    describe "analyse" $ do
        it "should give diagnostic on unused macros" $ do
            ast <- mustParse
                [ "#define SIZE 10"
                ]
            analyseGlobal ["callgraph"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:1: unused symbol `SIZE` [-Wcallgraph]"
                ]

        it "should not give diagnostics on symbols used in array dimensions" $ do
            ast <- mustParse
                [ "#define SIZE 10"
                , "struct Foo {"
                , "  char c[SIZE];"
                , "};"
                , "int main() { Foo foo; }"
                ]
            analyseGlobal ["callgraph"] [("test.c", ast)]
                `shouldBe`
                []

        it "should give diagnostics on undefined symbols used in array dimensions" $ do
            ast <- mustParse
                [ "typedef struct Foo {"
                , "  char c[SIZE];"
                , "} Foo;"
                , "int main() { Foo foo; }"
                ]
            analyseGlobal ["callgraph"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:2: definition of `Foo` references undefined global function/constant `SIZE` [-Wcallgraph]"
                ]
