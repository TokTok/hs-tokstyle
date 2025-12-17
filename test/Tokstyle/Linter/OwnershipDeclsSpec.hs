{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.OwnershipDeclsSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseGlobal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "allows qualifiers on declarations" $ do
        ast <- mustParse
            [ "int * _Owner foo(void);"
            , "int * _Nullable bar(void);"
            , "int * _Nonnull baz(void);"
            ]
        analyseGlobal ["ownership-decls"] [("test.c", ast)]
            `shouldBe`
            []

    it "allows qualifiers on parameters in declarations" $ do
        ast <- mustParse
            [ "void foo(int * _Owner p);"
            , "void bar(int * _Nullable p);"
            , "void baz(int * _Nonnull p);"
            ]
        analyseGlobal ["ownership-decls"] [("test.c", ast)]
            `shouldBe`
            []

    it "forbids qualifiers on global definitions" $ do
        ast <- mustParse
            [ "int * _Owner foo(void) { return nullptr; }"
            ]
        analyseGlobal ["ownership-decls"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:1: qualifier `_Owner` should only be used on function declarations, not definitions [-Wownership-decls]"
            ]

    it "reports multiple qualifiers when present" $ do
        ast <- mustParse
            [ "int * _Owner _Nullable foo(void) { return nullptr; }"
            ]
        analyseGlobal ["ownership-decls"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:1: qualifiers `_Owner` and `_Nullable` should only be used on function declarations, not definitions [-Wownership-decls]"
            ]

    it "forbids qualifiers on global definitions even if declared before" $ do
        ast <- mustParse
            [ "int * _Owner foo(void);"
            , "int * _Owner foo(void) { return nullptr; }"
            ]
        analyseGlobal ["ownership-decls"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:2: qualifier `_Owner` should only be used on function declarations, not definitions [-Wownership-decls]"
            ]

    it "forbids qualifiers on static definitions if declared before" $ do
        ast <- mustParse
            [ "static int * _Owner foo(void);"
            , "static int * _Owner foo(void) { return nullptr; }"
            ]
        analyseGlobal ["ownership-decls"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:2: qualifier `_Owner` should only be used on function declarations, not definitions [-Wownership-decls]"
            ]

    it "allows qualifiers on static definitions if NOT declared before" $ do
        ast <- mustParse
            [ "static int * _Owner foo(void) { return nullptr; }"
            ]
        analyseGlobal ["ownership-decls"] [("test.c", ast)]
            `shouldBe`
            []

    it "forbids qualifiers on parameters in definitions" $ do
        ast <- mustParse
            [ "void foo(int * _Owner p) { return; }"
            ]
        analyseGlobal ["ownership-decls"] [("test.c", ast)]
            `shouldBe`
            [ "test.c:1: qualifier `_Owner` should only be used on function declarations, not definitions [-Wownership-decls]"
            ]

-- end of tests
