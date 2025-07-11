{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.TypedefNameSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "should not give diagnostics on valid typedef names" $ do
        ast <- mustParse
            [ "typedef struct Foo Foo;"
            , "typedef struct Bar_s Bar_t;"
            ]
        analyseLocal ["typedef-name"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid typedef names" $ do
        ast <- mustParse
            [ "typedef struct Foo_s Bar_t;"
            ]
        analyseLocal ["typedef-name"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: typedef name `Bar_t` does not match struct name `Foo_s` [-Wtypedef-name]"
            ]

    it "should not give diagnostics on valid union names" $ do
        ast <- mustParse
            [ "typedef union Foo { int a; } Foo;"
            , "typedef union Bar_u { int a; } Bar_t;"
            ]
        analyseLocal ["typedef-name"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid union names" $ do
        ast <- mustParse
            [ "typedef union Foo_u { int a; } Bar_t;"
            ]
        analyseLocal ["typedef-name"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: typedef name `Bar_t` does not match union name `Foo_u` [-Wtypedef-name]"
            ]

    it "should not give diagnostics on valid enum names" $ do
        ast <- mustParse
            [ "typedef enum Foo { FOO } Foo;"
            , "typedef enum Bar_e { BAR } Bar_t;"
            ]
        analyseLocal ["typedef-name"] ("test.c", ast)
            `shouldBe`
            []

    it "should give diagnostics on invalid enum names" $ do
        ast <- mustParse
            [ "typedef enum Foo_e { FOO } Bar_t;"
            ]
        analyseLocal ["typedef-name"] ("test.c", ast)
            `shouldBe`
            [ "test.c:1: typedef name `Bar_t` does not match union name `Foo_e` [-Wtypedef-name]"
            ]
