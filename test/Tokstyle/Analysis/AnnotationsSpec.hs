{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Analysis.AnnotationsSpec where

import           Data.Map.Strict                            (Map)
import qualified Data.Map.Strict                            as Map
import           Data.Text                                  (Text)
import           Test.Hspec
import           Tokstyle.Analysis.SecurityRank.Annotations
import           Tokstyle.Analysis.SecurityRank.Lattice     (SecurityRank (..))
import           Tokstyle.LinterSpec                        (mustParse)

spec :: Spec
spec = do
    describe "parseAllAnnotations" $ do
        it "parses a simple source annotation" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message);"
                ]
            let expected = Map.fromList
                    [ ("send_message", Map.fromList [("source:message", Rank 0)]) ]
            parseAllAnnotations [("test.c", ast)] `shouldBe` expected

        it "parses a simple sink annotation" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                ]
            let expected = Map.fromList
                    [ ("network_send", Map.fromList [("sink:data", Rank 1)]) ]
            parseAllAnnotations [("test.c", ast)] `shouldBe` expected

        it "parses multiple annotations on one function" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, plain, 0)"
                , " * @security_rank(source, encrypted, 1)"
                , " */"
                , "void encrypt(const uint8_t *plain, uint8_t *encrypted);"
                ]
            let expected = Map.fromList
                    [ ("encrypt", Map.fromList [ ("sink:plain", Rank 0)
                                               , ("source:encrypted", Rank 1)
                                               ])
                    ]
            parseAllAnnotations [("test.c", ast)] `shouldBe` expected

        it "parses a return value annotation" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(source, return, 0)"
                , " */"
                , "const uint8_t* get_message();"
                ]
            let expected = Map.fromList
                    [ ("get_message", Map.fromList [("source:return", Rank 0)]) ]
            parseAllAnnotations [("test.c", ast)] `shouldBe` expected

        it "handles functions with no annotations" $ do
            ast <- mustParse
                [ "void do_nothing();"
                ]
            parseAllAnnotations [("test.c", ast)] `shouldBe` Map.empty

        it "parses annotations on a function definition" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data) { return; }"
                ]
            let expected = Map.fromList
                    [ ("network_send", Map.fromList [("sink:data", Rank 1)]) ]
            parseAllAnnotations [("test.c", ast)] `shouldBe` expected

        it "parses annotations on a struct member" $ do
            ast <- mustParse
                [ "struct Message {"
                , "  /** @security_rank(source, 1) */"
                , "  const uint8_t* ciphertext;"
                , "};"
                ]
            let expected = Map.fromList
                    [ ("Message.ciphertext", Map.fromList [("source", Rank 1)]) ]
            parseAllAnnotations [("test.c", ast)] `shouldBe` expected

        it "parses a sink_max annotation" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " * @security_rank(sink_max, data, 2)"
                , " */"
                , "void network_send(const uint8_t *data);"
                ]
            let expected = Map.fromList
                    [ ("network_send", Map.fromList [ ("sink:data", Rank 1)
                                                    , ("sink_max:data", Rank 2)
                                                    ])
                    ]
            parseAllAnnotations [("test.c", ast)] `shouldBe` expected

        it "parses annotations on a typedef function pointer" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, message, 1)"
                , " */"
                , "typedef void sending_cb(const uint8_t* message);"
                ]
            let expected = Map.fromList
                    [ ("sending_cb", Map.fromList [("sink:message", Rank 1)]) ]
            parseAllAnnotations [("test.c", ast)] `shouldBe` expected

        it "parses multiple annotations on a struct member" $ do
            ast <- mustParse
                [ "struct Message {"
                , "  /**"
                , "   * @security_rank(source, 1)"
                , "   * @security_rank(encrypted, 1)"
                , "   */"
                , "  const uint8_t* ciphertext;"
                , "};"
                ]
            let expected = Map.fromList
                    [ ("Message.ciphertext", Map.fromList [ ("source", Rank 1)
                                                          , ("encrypted", Rank 1)
                                                          ])
                    ]
            parseAllAnnotations [("test.c", ast)] `shouldBe` expected

        it "parses annotations inside a typedef struct" $ do
            ast <- mustParse
                [ "typedef struct Message {"
                , "  /** @security_rank(source, 0) */"
                , "  const uint8_t* plaintext;"
                , "} Message_t;"
                ]
            let expected = Map.fromList
                    [ ("Message.plaintext", Map.fromList [("source", Rank 0)]) ]
            parseAllAnnotations [("test.c", ast)] `shouldBe` expected

        it "parses an annotation on the line above a struct member" $ do
            ast <- mustParse
                [ "struct MyTestStruct {"
                , "    /** @security_rank(source, 1) */"
                , "    int annotated_above;"
                , "};"
                ]
            let expected = Map.fromList
                    [ ("MyTestStruct.annotated_above", Map.fromList [("source", Rank 1)])
                    ]
            parseAllAnnotations [("test.c", ast)] `shouldBe` expected
