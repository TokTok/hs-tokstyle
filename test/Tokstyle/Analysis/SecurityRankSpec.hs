{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Analysis.SecurityRankSpec where

import           Test.Hspec
import           Tokstyle.Linter     (analyseGlobal)
import           Tokstyle.LinterSpec (mustParse)

spec :: Spec
spec = do
    describe "SecurityRank Analysis" $ do
        it "warns about sending tainted data directly to a sink" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(int data);"
                , ""
                , "/** @security_rank(source, p, 0) */"
                , "void get_tainted(int* p);"
                , ""
                , "void main() {"
                , "  int x;"
                , "  get_tainted(&x);"
                , "  network_send(x);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` ["test.c:10: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"]

        it "accepts un-tainted data sent to a sink" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "void send_untainted() {"
                , "  const uint8_t *untainted_data = (const uint8_t *)\"hello\";"
                , "  network_send(untainted_data);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "warns when tainted data is sent to a sink via a struct's function pointer" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(int data);"
                , ""
                , "/** @security_rank(source, p, 0) */"
                , "void get_tainted(int* p);"
                , ""
                , "typedef void send_cb(int x);"
                , ""
                , "typedef struct Function_Table {"
                , "    send_cb *send_callback;"
                , "} Function_Table;"
                , ""
                , "void process_request(Function_Table* d) {"
                , "    int tainted_val;"
                , "    get_tainted(&tainted_val);"
                , "    d->send_callback(tainted_val);"
                , "}"
                , ""
                , "void main_logic() {"
                , "    Function_Table f_table;"
                , "    f_table.send_callback = &network_send;"
                , "    process_request(&f_table);"
                , "}"
                ]
            pendingWith "context-sensitive analysis"
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` ["test.c:16: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"]
