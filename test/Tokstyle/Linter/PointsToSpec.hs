{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.PointsToSpec where

import           Test.Hspec          (Spec, describe, it, shouldBe)

import           Tokstyle.Linter     (analyseGlobal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    describe "Points-To Linter" $ do
        it "warns when using the return value of an unsummarized function in an assignment" $ do
            ast <- mustParse
                [ "int* unknown_func();"
                , "void test() {"
                , "  int* p = unknown_func();"
                , "}"
                ]
            analyseGlobal ["points-to"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: The return value of function 'unknown_func' is used here, but its value could not be determined by the analysis. [-Wpoints-to]"
                ]

        it "warns when using the return value of an unsummarized function in a variable declaration" $ do
            ast <- mustParse
                [ "int* unknown_func();"
                , "void test() {"
                , "  int* p;"
                , "  p = unknown_func();"
                , "}"
                ]
            analyseGlobal ["points-to"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:4: The return value of function 'unknown_func' is used here, but its value could not be determined by the analysis. [-Wpoints-to]"
                ]

        it "does not warn when the return value is not used" $ do
            ast <- mustParse
                [ "int* unknown_func();"
                , "void test() {"
                , "  unknown_func();"
                , "}"
                ]
            analyseGlobal ["points-to"] [("test.c", ast)] `shouldBe` []

        it "does not warn for summarized functions like malloc" $ do
            ast <- mustParse
                [ "void* malloc(unsigned long size);"
                , "void test() {"
                , "  int* p = (int*)malloc(sizeof(int));"
                , "}"
                ]
            analyseGlobal ["points-to"] [("test.c", ast)] `shouldBe` []

        it "warns when returning the value of an unsummarized function" $ do
            ast <- mustParse
                [ "int* unknown_func();"
                , "int* test() {"
                , "  return unknown_func();"
                , "}"
                ]
            analyseGlobal ["points-to"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: The return value of function 'unknown_func' is used here, but its value could not be determined by the analysis. [-Wpoints-to]"
                ]

        it "does not warn for functions defined in the TU" $ do
            ast <- mustParse
                [ "int* defined_func() { return 0; }"
                , "void test() {"
                , "  int* p = defined_func();"
                , "}"
                ]
            analyseGlobal ["points-to"] [("test.c", ast)] `shouldBe` []

        it "warns when using a casted return value of an unsummarized function" $ do
            ast <- mustParse
                [ "int* unknown_func();"
                , "void test() {"
                , "  void* p = (void*)unknown_func();"
                , "}"
                ]
            analyseGlobal ["points-to"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:3: The return value of function 'unknown_func' is used here, but its value could not be determined by the analysis. [-Wpoints-to]"
                ]

        it "warns on indirect calls through a function pointer (limitation)" $ do
            ast <- mustParse
                [ "typedef int* func_ptr_cb(void);"
                , "int* unknown_func();"
                , "void test() {"
                , "  func_ptr_cb* fp;"
                , "  fp = unknown_func;"
                , "  int* p = fp();"
                , "}"
                ]
            analyseGlobal ["points-to"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:6: The return value of function 'fp' is used here, but its value could not be determined by the analysis. [-Wpoints-to]"
                ]

        it "does not warn when an indirect call can be resolved" $ do
            ast <- mustParse
                [ "typedef int callback_cb(void);"
                , "int my_callback() { return 1; }"
                , "void uses_callback(callback_cb* cb) {"
                , "  int x = cb();"
                , "}"
                , "void test() {"
                , "  uses_callback(my_callback);"
                , "}"
                ]
            analyseGlobal ["points-to"] [("test.c", ast)] `shouldBe` []

        it "does not warn when taking the address of an unsummarized function" $ do
            ast <- mustParse
                [ "typedef int* func_ptr_cb(void);"
                , "int* unknown_func();"
                , "void test() {"
                , "  func_ptr_cb* fp = unknown_func;"
                , "}"
                ]
            analyseGlobal ["points-to"] [("test.c", ast)] `shouldBe` []

        it "does not warn when an unsummarized function is passed as an argument" $ do
            ast <- mustParse
                [ "typedef int* func_ptr_cb(void);"
                , "void register_callback(func_ptr_cb* cb);"
                , "int* unknown_func();"
                , "void test() {"
                , "  register_callback(unknown_func);"
                , "}"
                ]
            analyseGlobal ["points-to"] [("test.c", ast)] `shouldBe` []

        it "does not warn for locally-defined functions with prototypes" $ do
            ast <- mustParse
                [ "int* my_func();"
                , "void test() {"
                , "  int* p = my_func();"
                , "}"
                , "int* my_func() { return nullptr; }"
                ]
            analyseGlobal ["points-to"] [("test.c", ast)]
                `shouldBe`
                []

        it "does not warn for toxencryptsave when the definition is available in another TU" $ do
            astH <- mustParse -- Header
                [ "typedef struct Tox_Pass_Key Tox_Pass_Key;"
                , "struct Tox_Pass_Key { int a; };"
                , "typedef enum Tox_Err_Key_Derivation { TOX_ERR_KEY_DERIVATION_OK } Tox_Err_Key_Derivation;"
                , "Tox_Pass_Key *tox_pass_key_derive(const uint8_t passphrase[], size_t passphrase_len, Tox_Err_Key_Derivation *error);"
                , "Tox_Pass_Key *tox_pass_key_derive_with_salt(const uint8_t passphrase[], size_t passphrase_len, const uint8_t salt[], Tox_Err_Key_Derivation *error);"
                ]
            astC <- mustParse -- Source
                [ -- Simulating inclusion of the header by passing both ASTs to the linter
                  "Tox_Pass_Key *tox_pass_key_derive(const uint8_t passphrase[], size_t passphrase_len, Tox_Err_Key_Derivation *error) {"
                , "  uint8_t salt[32];"
                , "  return tox_pass_key_derive_with_salt(passphrase, passphrase_len, salt, error);"
                , "}"
                , "Tox_Pass_Key *tox_pass_key_derive_with_salt(const uint8_t passphrase[], size_t passphrase_len, const uint8_t salt[], Tox_Err_Key_Derivation *error) {"
                , "  return nullptr;"
                , "}"
                ]
            analyseGlobal ["points-to"] [("test.h", astH), ("test.c", astC)]
                `shouldBe`
                []
