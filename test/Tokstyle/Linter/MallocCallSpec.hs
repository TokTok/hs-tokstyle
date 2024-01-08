{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.MallocCallSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (allWarnings, analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "warns when mem_alloc() is used outside a var decl stmt" $ do
        ast <- mustParse
            [ "int a(My_Struct **v) {"
            , "  *v = (My_Struct *)mem_alloc(mem, sizeof(My_Struct));"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            ["test.c:2: allocations using `mem_alloc` must first be assigned to a local variable or returned directly [-Wmalloc-call]"]

    it "warns if there is no null-check after an allocation" $ do
        ast <- mustParse
            [ "void a(My_Struct **v) {"
            , "  My_Struct *tmp = (My_Struct *)mem_alloc(mem, sizeof(My_Struct));"
            , "  *v = tmp;"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe` ["test.c:3: `tmp`, assigned from `mem_alloc` must immediately be checked against `nullptr` [-Wmalloc-call]"]

    it "accepts mem_alloc() being used in a var decl stmt" $ do
        ast <- mustParse
            [ "bool a(My_Struct **v) {"
            , "  My_Struct *tmp = (My_Struct *)mem_alloc(mem, sizeof(My_Struct));"
            , "  if (tmp == nullptr) {"
            , "    return false;"
            , "  }"
            , "  *v = tmp;"
            , "  return true;"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe` []

    it "accepts mem_alloc() being used in a return statement" $ do
        ast <- mustParse
            [ "My_Struct *my_struct_new(void) {"
            , "  return (My_Struct *)mem_alloc(mem, sizeof(My_Struct));"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe` []
