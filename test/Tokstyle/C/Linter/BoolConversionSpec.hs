{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.BoolConversionSpec (spec) where

import           Test.Hspec            (Spec, describe, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings, analyse)
import           Tokstyle.C.LinterSpec (mustParse)


spec :: Spec
spec = describe "BoolConversion linter" $ do
    describe "implicit conversion to bool" $ do
        it "warns on ternary operators" $ do
            ast <- mustParse
                [ "void func(char *p) {"
                , "  int i = p ? 1 : 0;"
                , "}"
                ]
            analyse allWarnings ast
                `shouldBe`
                [ Text.unlines
                    [ "test.c:2: (column 11) [ERROR]  >>> Type mismatch"
                    , "  implicit conversion from char * to bool"
                    ]
                ]

        it "warns on logical not" $ do
            ast <- mustParse
                [ "void func(char *p) {"
                , "  !p;"
                , "}"
                ]
            analyse allWarnings ast
                `shouldBe`
                [ Text.unlines
                    [ "test.c:2: (column 4) [ERROR]  >>> Type mismatch"
                    , "  implicit conversion from char * to bool"
                    ]
                ]

        it "warns on logical or" $ do
            ast <- mustParse
                [ "void func(char *p) {"
                , "  p || p;"
                , "}"
                ]
            analyse allWarnings ast
                `shouldBe`
                [ Text.unlines
                    [ "test.c:2: (column 3) [ERROR]  >>> Type mismatch"
                    , "  implicit conversion from char * to bool"
                    ]
                , Text.unlines
                    [ "test.c:2: (column 8) [ERROR]  >>> Type mismatch"
                    , "  implicit conversion from char * to bool"
                    ]
                ]

        it "warns on logical and" $ do
            ast <- mustParse
                [ "void func(char *p) {"
                , "  p && p;"
                , "}"
                ]
            analyse allWarnings ast
                `shouldBe`
                [ Text.unlines
                    [ "test.c:2: (column 3) [ERROR]  >>> Type mismatch"
                    , "  implicit conversion from char * to bool"
                    ]
                , Text.unlines
                    [ "test.c:2: (column 8) [ERROR]  >>> Type mismatch"
                    , "  implicit conversion from char * to bool"
                    ]
                ]

    describe "valid bool expressions" $ do
        it "allows integer literals" $ do
            ast <- mustParse
                [ "void func() {"
                , "  if (1) {}"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []

        it "allows boolean expressions" $ do
            ast <- mustParse
                [ "void func(int i) {"
                , "  if (i == 1) {}"
                , "}"
                ]
            analyse allWarnings ast `shouldBe` []
