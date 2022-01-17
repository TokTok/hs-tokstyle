{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.VarUnusedInScopeSpec where

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
        it "should give diagnostics on vars that can be reduced in scope" $ do
            ast <- mustParse
                [ "int a(void) {"
                , "  int i;"
                , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
                , "}"
                ]
            analyse allWarnings ("test.c", ast)
                `shouldBe`
                [ "test.c:2: variable `i' can be reduced in scope [-Wvar-unused-in-scope]"
                , "test.c:3:   possibly to here [-Wvar-unused-in-scope]"
                ]

        it "should support #if/#endif" $ do
            ast <- mustParse
                [ "int a(void) {"
                , "#if HAHA"
                , "  int i;"
                , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
                , "#endif"
                , "}"
                ]
            analyse allWarnings ("test.c", ast)
                `shouldBe`
                [ "test.c:3: variable `i' can be reduced in scope [-Wvar-unused-in-scope]"
                , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
                ]

        it "should support #if/#else/#endif" $ do
            ast <- mustParse
                [ "int a(void) {"
                , "#if HAHA"
                , "  int i;"
                , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
                , "#else"
                , "  int i;"
                , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
                , "#endif"
                , "}"
                ]
            analyse allWarnings ("test.c", ast)
                `shouldBe`
                [ "test.c:6: variable `i' can be reduced in scope [-Wvar-unused-in-scope]"
                , "test.c:7:   possibly to here [-Wvar-unused-in-scope]"
                , "test.c:3: variable `i' can be reduced in scope [-Wvar-unused-in-scope]"
                , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
                ]

        it "should detect multiple uses, as long as all of them are writes" $ do
            ast <- mustParse
                [ "int a(void) {"
                , "  int i;"
                , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
                , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
                , "}"
                ]
            analyse allWarnings ("test.c", ast)
                `shouldBe`
                [ "test.c:2: variable `i' can be reduced in scope [-Wvar-unused-in-scope]"
                , "test.c:3:   possibly to here [-Wvar-unused-in-scope]"
                ]

        it "should work on variables declared multiple scopes up" $ do
            ast <- mustParse
                [ "int a(void) {"
                , "  int i;"
                , "  if (true) {"
                , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
                , "    print_int(i);"
                , "  }"
                , "}"
                ]
            analyse allWarnings ("test.c", ast)
                `shouldBe`
                [ "test.c:2: variable `i' can be reduced in scope [-Wvar-unused-in-scope]"
                , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
                ]

        it "should work on variables only-written in both if branches" $ do
            ast <- mustParse
                [ "int a(void) {"
                , "  int i;"
                , "  if (true) {"
                , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
                , "  } else {"
                , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
                , "  }"
                , "}"
                ]
            analyse allWarnings ("test.c", ast)
                `shouldBe`
                [ "test.c:2: variable `i' can be reduced in scope [-Wvar-unused-in-scope]"
                , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
                ]

        it "should not diagnose variables that are read in one of the branches" $ do
            ast <- mustParse
                [ "int a(void) {"
                , "  int i;"
                , "  if (true) {"
                , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
                , "  } else {"
                , "    print_int(i);"
                , "  }"
                , "}"
                ]
            analyse allWarnings ("test.c", ast) `shouldBe` []

        it "should not give diagnostics on vars read in the same scope" $ do
            ast <- mustParse
                [ "int a(void) {"
                , "  int i;"
                , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); blah(); }"
                , "  print_int(i);"
                , "}"
                ]
            analyse allWarnings ("test.c", ast) `shouldBe` []

        it "should not give diagnostics on vars used as the bound for another for-loop" $ do
            ast <- mustParse
                [ "int a(void) {"
                , "  int i;"
                , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); blah(); }"
                , "  for (int j = 0; j < i; ++j) { puts(\"hello!\"); }"
                , "}"
                ]
            analyse allWarnings ("test.c", ast) `shouldBe` []

        it "should not give diagnostics on assignments on array index operations" $ do
            ast <- mustParse
                [ "int a(char *p) {"
                , "  char *c = p;"
                , "  if (true) { c[0] = 'a'; }"
                , "}"
                ]
            analyse allWarnings ("test.c", ast) `shouldBe` []
