{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.Linter.SizeofSpec (spec) where

import           Test.Hspec            (Spec, it, shouldBe)

import qualified Data.Text             as Text
import           Tokstyle.C.Linter     (allWarnings, analyse)
import           Tokstyle.C.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should give diagnostics on passing pointers to sizeof" $ do
        ast <- mustParse
            [ "int foo(void) {"
            , "  char *ptr;"
            , "  return sizeof ptr;"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:3: (column 17) [ERROR]  >>> Type mismatch"
                , "  disallowed sizeof argument of type `char *` - did you mean for `ptr` to be an array?"
                ]
            ]

    it "should give diagnostics on passing array-element pointers to sizeof" $ do
        ast <- mustParse
            [ "int foo(void) {"
            , "  char arr[10];"
            , "  return sizeof &arr[0];"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:3: (column 17) [ERROR]  >>> Type mismatch"
                , "  disallowed sizeof argument of type `char *` - did you mean for `&arr[0]` to be an array?"
                ]
            ]

    it "should not diagnostics on passing arrays to sizeof" $ do
        ast <- mustParse
            [ "int foo(void) {"
            , "  char arr[10];"
            , "  return sizeof arr;"
            , "}"
            ]
        analyse allWarnings ast
            `shouldBe` []
