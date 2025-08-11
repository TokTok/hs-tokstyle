{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.SecurityRankSpec where

import           Test.Hspec          (Spec, describe, it, pendingWith, shouldBe)

import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Tokstyle.Linter     (analyseGlobal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    describe "acceptance" $ do
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

        it "accepts overwriting a tainted variable with a safe value" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  const uint8_t *my_message = message;"
                , "  my_message = (const uint8_t *)\"safe\";"
                , "  network_send(my_message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts safe data in if branch" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message, int condition) {"
                , "  if (condition) {"
                , "    network_send((const uint8_t *)\"safe\");"
                , "  } else {"
                , "    const uint8_t* temp = message;"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts matching ranks (1 -> 1)" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 1)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  network_send(message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts higher rank source to lower rank sink (1 -> 0)" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 0)"
                , " */"
                , "void log_message(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 1)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  log_message(message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts data flow through a struct without tainting other fields" $ do
            ast <- mustParse
                [ "typedef struct Message { const uint8_t *sensitive; const uint8_t *nonsensitive; } Message;"
                , ""
                , "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message_data, 0)"
                , " */"
                , "void send_message(const uint8_t *message_data) {"
                , "  Message msg;"
                , "  msg.sensitive = message_data;"
                , "  msg.nonsensitive = (const uint8_t *)\"safe\";"
                , "  network_send(msg.nonsensitive);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts safe return value from function (inferred)" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "const uint8_t* get_message() {"
                , "  return (const uint8_t*)\"safe\";"
                , "}"
                , ""
                , "void send_message() {"
                , "  const uint8_t *my_message = get_message();"
                , "  network_send(my_message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts safe return value from function (annotated)" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, return, 1)"
                , " */"
                , "const uint8_t* get_message();"
                , ""
                , "void send_message() {"
                , "  const uint8_t *my_message = get_message();"
                , "  network_send(my_message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts multiple safe return paths" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, return, 1)"
                , " */"
                , "const uint8_t* get_message(int use_default) {"
                , "  if (use_default) {"
                , "    return (const uint8_t*)\"default\";"
                , "  }"
                , "  return (const uint8_t*)\"other\";"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts complex control flow where taint does not reach sink" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message, int condition1, int condition2) {"
                , "  const uint8_t* p;"
                , "  if (condition1) {"
                , "    p = message;"
                , "    if (condition2) {"
                , "      p = (const uint8_t*)\"safe\";"
                , "      network_send(p);"
                , "    }"
                , "  } else {"
                , "    p = (const uint8_t*)\"other safe\";"
                , "    network_send(p);"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts safe usage with goto" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message, int condition) {"
                , "  const uint8_t* p = message;"
                , "  if (condition) {"
                , "    goto SAFE;"
                , "  }"
                , "  goto RETURN;"
                , "SAFE:"
                , "  p = (const uint8_t*)\"safe\";"
                , "  network_send(p);"
                , "RETURN:"
                , "  return;"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts multiple safe returns from a nested function" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "const uint8_t* get_safe_message(int type) {"
                , "  if (type == 1) return (const uint8_t*)\"type 1 safe\";"
                , "  return (const uint8_t*)\"default safe\";"
                , "}"
                , ""
                , "void process_and_send(int type) {"
                , "  const uint8_t* msg = get_safe_message(type);"
                , "  network_send(msg);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts clearing taint inside a loop before sink" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , "/** @security_rank(source, message, 0) */"
                , "void send_loop(const uint8_t *message) {"
                , "  const uint8_t* p = message;"
                , "  for (int i = 0; i < 2; ++i) {"
                , "    p = (const uint8_t*)\"safe\";"
                , "    network_send(p);"
                , "    if (i == 0) { p = message; }"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts safe return from a function with multiple complex return paths" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, return, 1) */"
                , "int get_value(int status, int value) {"
                , "  if (status == 0) {"
                , "    if (value > 100) {"
                , "      return 1;"
                , "    } else {"
                , "      return 2;"
                , "    }"
                , "  } else if (status == 1) {"
                , "    for (int i = 0; i < 5; ++i) {"
                , "      if (i == value) return 3;"
                , "    }"
                , "    return 4;"
                , "  }"
                , "  return 5;"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts taint sanitized in a switch statement" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , "/** @security_rank(source, message, 0) */"
                , "void process(const uint8_t *message, int type) {"
                , "  const uint8_t* p = message;"
                , "  switch(type) {"
                , "    case 0: {"
                , "      p = (const uint8_t*)\"safe0\";"
                , "      network_send(p);"
                , "      break;"
                , "    }"
                , "    case 1: {"
                , "      p = (const uint8_t*)\"safe1\";"
                , "      network_send(p);"
                , "      break;"
                , "    }"
                , "    default: {"
                , "      p = (const uint8_t*)\"safe_default\";"
                , "      network_send(p);"
                , "      break;"
                , "    }"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts safe data flow with pointer-to-pointer" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , "/** @security_rank(source, message, 0) */"
                , "void process(const uint8_t *message) {"
                , "  const uint8_t* safe_ptr = (const uint8_t*)\"safe\";"
                , "  const uint8_t** pptr = &safe_ptr;"
                , "  network_send(*pptr);"
                , "  const uint8_t* tainted_ptr = message;"
                , "  pptr = &tainted_ptr;"
                , "  safe_ptr = (const uint8_t*)\"still safe\";"
                , "  network_send(safe_ptr);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

--      it "accepts safe data flow in a while loop with break and continue" $ do
--          ast <- mustParse
--              [ "/** @security_rank(sink, data, 1) */"
--              , "void network_send(const uint8_t *data);"
--              , "/** @security_rank(source, message, 0) */"
--              , "void process(const uint8_t *message) {"
--              , "  int i = 0;"
--              , "  const uint8_t* p = message;"
--              , "  while(i < 10) {"
--              , "    ++i;"
--              , "    if (i < 5) {"
--              , "      p = (const uint8_t*)\"safe in loop\";"
--              , "      continue;"
--              , "    }"
--              , "    network_send(p);"
--              , "    if (i == 7) {"
--              , "      break;"
--              , "    }"
--              , "  }"
--              , "}"
--              ]
--          analyseGlobal ["security-rank"] [("test.c", ast)]
--              `shouldBe` []

        it "accepts safe data flow with mutual recursion" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(int data);"
                , "int f(int x);"
                , "int g(int x) {"
                , "  if (x > 0) return f(x - 1);"
                , "  return 10;"
                , "}"
                , "int f(int x) {"
                , "  if (x > 0) return g(x - 1);"
                , "  return 20;"
                , "}"
                , "void main() {"
                , "  int val = f(5);"
                , "  network_send(val);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "accepts taint propagation through an array without tainting other elements" $ do
            ast <- mustParse
                [ "/** @security_rank(source, data, 0) */"
                , "void source(char* data[]);"
                , "/** @security_rank(sink, data, 1) */"
                , "void sink(char* data);"
                , ""
                , "void main() {"
                , "    char* user_input[2];"
                , "    source(user_input);"
                , "    char* safe_data = \"safe\";"
                , "    user_input[1] = safe_data;"
                , "    sink(user_input[1]);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "allows higher rank data to be returned from a function returning a lower rank" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, return, 0)"
                , " * @security_rank(source, message, 1)"
                , " */"
                , "const uint8_t* get_message(const uint8_t *message, int use_default) {"
                , "  if (use_default) {"
                , "    return (const uint8_t*)\"default\";"
                , "  }"
                , "  return message;"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

    describe "struct member security" $ do
        it "accepts safe handling of a sensitive struct member" $ do
            ast <- mustParse
                [ "struct SecretContainer {"
                , "  /** @security_rank(source, 2) */"
                , "  const uint8_t* secret;"
                , "};"
                , ""
                , "/** @security_rank(sink, data, 2) */"
                , "void store_secret(const uint8_t* data);"
                , ""
                , "void handle_secret(struct SecretContainer* c) {"
                , "  store_secret(c->secret);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "warns when a sensitive struct member is leaked to a low-rank sink" $ do
            ast <- mustParse
                [ "struct SecretContainer {"
                , "  /** @security_rank(source, 1) */"
                , "  const uint8_t* secret;"
                , "};"
                , ""
                , "/**"
                , " * @security_rank(sink, data, 2)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "void handle_secret(struct SecretContainer* c) {"
                , "  network_send(c->secret);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: security risk: tainted data of rank Rank 1 sent to sink of rank Rank 2 [-Wsecurity-rank]"
                ]

        it "accepts sending an encrypted member to a network sink" $ do
            ast <- mustParse
                [ "struct Message {"
                , "  const uint8_t* plaintext;"
                , "  /** @security_rank(source, 1) */"
                , "  const uint8_t* ciphertext;"
                , "};"
                , ""
                , "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "void send_it(struct Message* m) {"
                , "  network_send(m->ciphertext);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "warns when a plaintext member is sent to a network sink" $ do
            ast <- mustParse
                [ "struct Message {"
                , "  /** @security_rank(source, 0) */"
                , "  const uint8_t* plaintext;"
                , "  const uint8_t* ciphertext;"
                , "};"
                , ""
                , "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "void send_it(struct Message* m) {"
                , "  network_send(m->plaintext);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:13: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

    describe "struct with function pointers" $ do
        it "warns when tainted data is sent to a sink via a struct's function pointer" $ do
            ast <- mustParse
                [ "typedef void sending_cb(const uint8_t* message);"
                , ""
                , "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/** @security_rank(source, message, 0) */"
                , "const uint8_t* get_tainted_message(const uint8_t* message) { return message; }"
                , ""
                , "typedef struct Dispatcher {"
                , "  sending_cb *send_callback;"
                , "} Dispatcher;"
                , ""
                , "void process_request(Dispatcher* d, const uint8_t* data) {"
                , "  d->send_callback(data);"
                , "}"
                , ""
                , "/** @security_rank(source, raw_message, 0) */"
                , "void main_logic(const uint8_t* raw_message) {"
                , "  Dispatcher my_dispatcher;"
                , "  my_dispatcher.send_callback = &network_send;"
                , "  const uint8_t* tainted_data = get_tainted_message(raw_message);"
                , "  process_request(&my_dispatcher, tainted_data);"
                , "}"
                ]
            pendingWith "context-sensitive analysis"
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:22: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "accepts when safe data is sent to a sink via a struct's function pointer" $ do
            ast <- mustParse
                [ "typedef void sending_cb(const uint8_t* message);"
                , ""
                , "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "typedef struct Dispatcher {"
                , "  sending_cb *send_callback;"
                , "} Dispatcher;"
                , ""
                , "void process_request(Dispatcher* d, const uint8_t* data) {"
                , "  d->send_callback(data);"
                , "}"
                , ""
                , "void main_logic_safe() {"
                , "  Dispatcher my_dispatcher;"
                , "  my_dispatcher.send_callback = &network_send;"
                , "  const uint8_t* safe_data = (const uint8_t*)\"safe\";"
                , "  process_request(&my_dispatcher, safe_data);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "warns when assigning an incompatible function to a function pointer field" $ do
            ast <- mustParse
                [ "typedef void sending_cb(const uint8_t* message);"
                , ""
                , "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/** @security_rank(sink, data, 0) */"
                , "void log_message(const uint8_t *data);"
                , ""
                , "typedef struct Dispatcher {"
                , "  sending_cb *send_callback;"
                , "} Dispatcher;"
                , ""
                , "void configure_dispatcher(Dispatcher* d, int use_logging) {"
                , "  d->send_callback = &network_send;"
                , "  if (use_logging) {"
                , "    d->send_callback = &log_message;"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:16: security risk: incompatible function signature assigned to function pointer; expected sink rank Rank 1 for argument 0, but got rank Rank 0 [-Wsecurity-rank]"
                ]

        it "accepts assigning a compatible function to a function pointer field" $ do
            ast <- mustParse
                [ "typedef void sending_cb(const uint8_t* message);"
                , ""
                , "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/** @security_rank(sink, data, 1) */"
                , "void another_network_send(const uint8_t *data);"
                , ""
                , "typedef struct Dispatcher {"
                , "  sending_cb *send_callback;"
                , "} Dispatcher;"
                , ""
                , "void configure_dispatcher(Dispatcher* d, int use_alt) {"
                , "  d->send_callback = &network_send;"
                , "  if (use_alt) {"
                , "    d->send_callback = &another_network_send;"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

    describe "failures" $ do
        it "warns about sending tainted data directly to a sink" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  network_send(message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:10: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data propagated through a variable" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  const uint8_t *my_message = message;"
                , "  network_send(my_message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:11: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data propagated through a function call" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "void helper_send(const uint8_t *data) {"
                , "  network_send(data);"
                , "}"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  helper_send(message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:14: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data propagated through a struct" $ do
            ast <- mustParse
                [ "typedef struct Message { const uint8_t *data; } Message;"
                , ""
                , "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message_data, 0)"
                , " */"
                , "void send_message(const uint8_t *message_data) {"
                , "  Message msg;"
                , "  msg.data = message_data;"
                , "  network_send(msg.data);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:14: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "considers decrypted data as tainted" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(sink, encrypted, 1)"
                , " * @security_rank(source, plain, 0)"
                , " */"
                , "void decrypt_data_symmetric(const uint8_t *encrypted, uint8_t *plain);"
                , ""
                , "void forward_message(const uint8_t *encrypted_message) {"
                , "  uint8_t plain_message[1024];"
                , "  decrypt_data_symmetric(encrypted_message, plain_message);"
                , "  network_send(plain_message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:15: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data propagated through multiple function calls" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "void helper_send2(const uint8_t *data) {"
                , "  network_send(data);"
                , "}"
                , ""
                , "void helper_send1(const uint8_t *data) {"
                , "  helper_send2(data);"
                , "}"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  helper_send1(message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:18: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data propagated through a ternary operator" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message, int condition) {"
                , "  network_send(condition ? message : (const uint8_t *)\"safe\");"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:10: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data in if branch" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message, int condition) {"
                , "  if (condition) {"
                , "    network_send(message);"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:11: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data in while loop" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  int i = 0;"
                , "  while (i < 1) {"
                , "    network_send(message);"
                , "    ++i;"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data in for loop" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  for (int i = 0; i < 1; ++i) {"
                , "    network_send(message);"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:11: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data in do-while loop" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  int i = 0;"
                , "  do {"
                , "    network_send(message);"
                , "    ++i;"
                , "  } while (i < 1);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data in switch case" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message, int option) {"
                , "  switch (option) {"
                , "    case 1: {"
                , "      network_send(message);"
                , "      break;"
                , "    }"
                , "    default: {"
                , "      break;"
                , "    }"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data reached via goto" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message, int condition) {"
                , "  if (condition) {"
                , "    goto SEND_MESSAGE;"
                , "  }"
                , "  return;"
                , "SEND_MESSAGE:"
                , "  network_send(message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:15: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted return value from function (inferred)" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "const uint8_t* get_message(const uint8_t *message) {"
                , "  return message;"
                , "}"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  const uint8_t *my_message = get_message(message);"
                , "  network_send(my_message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:15: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted return value from function (annotated)" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, return, 0)"
                , " */"
                , "const uint8_t* get_message();"
                , ""
                , "void send_message() {"
                , "  const uint8_t *my_message = get_message();"
                , "  network_send(my_message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:13: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted local variable returned from function" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "const uint8_t* get_message(const uint8_t *message) {"
                , "  const uint8_t* local_message = message;"
                , "  return local_message;"
                , "}"
                , ""
                , "void send_message(const uint8_t *message) {"
                , "  const uint8_t *my_message = get_message(message);"
                , "  network_send(my_message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:16: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about rank mismatch (1 -> 2)" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 2)"
                , " */"
                , "void high_sec_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 1)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  high_sec_send(message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:10: security risk: tainted data of rank Rank 1 sent to sink of rank Rank 2 [-Wsecurity-rank]"
                ]

        it "warns about tainted data propagated through aliased pointer" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  const uint8_t *p1 = message;"
                , "  const uint8_t *p2 = p1;"
                , "  network_send(p2);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "propagates taint through binary operations" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(int data);"
                , ""
                , "/**"
                , " * @security_rank(source, val, 0)"
                , " */"
                , "void send_value(int val) {"
                , "  int x = val + 1;"
                , "  network_send(x);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:11: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data from one of multiple return paths" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, return, 1)"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "const uint8_t* get_message(const uint8_t *message, int use_default) {"
                , "  if (use_default) {"
                , "    return (const uint8_t*)\"default\";"
                , "  }"
                , "  return message;"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:9: security risk: tainted data of rank Rank 0 returned from function with sink rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data from function with multiple returns" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "const uint8_t* get_message(const uint8_t *message, int use_default) {"
                , "  if (use_default) {"
                , "    return (const uint8_t*)\"default\";"
                , "  }"
                , "  return message;"
                , "}"
                , ""
                , "void send_message(const uint8_t* message, int use_default) {"
                , "  const uint8_t* m = get_message(message, use_default);"
                , "  network_send(m);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:18: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data in nested control flow" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message, int condition1, int condition2) {"
                , "  for (int i = 0; i < 1; ++i) {"
                , "    if (condition1) {"
                , "      if (condition2) {"
                , "        network_send(message);"
                , "      }"
                , "    }"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:13: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data in switch with fall-through" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message, int option) {"
                , "  const uint8_t* p = (const uint8_t*)\"safe\";"
                , "  switch (option) {"
                , "    case 0: {"
                , "      p = message;"
                , "      // fallthrough"
                , "    }"
                , "    case 1: {"
                , "      network_send(p);"
                , "      break;"
                , "    }"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:17: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data with continue and break" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message, int condition) {"
                , "  for (int i = 0; i < 10; ++i) {"
                , "    if (i < 5) {"
                , "      continue;"
                , "    }"
                , "    network_send(message);"
                , "    if (condition) {"
                , "      break;"
                , "    }"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:14: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data in later loop iteration" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 1)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "void send_message(const uint8_t *message) {"
                , "  const uint8_t* p = (const uint8_t*)\"safe\";"
                , "  int i = 0;"
                , "  while (i < 2) {"
                , "    network_send(p);"
                , "    p = message;"
                , "    ++i;"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:13: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns when variable can have multiple ranks and one is too low" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, data, 2)"
                , " */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/**"
                , " * @security_rank(source, message1, 0)"
                , " * @security_rank(source, message2, 2)"
                , " */"
                , "void send_message(const uint8_t *message1, const uint8_t* message2, int condition) {"
                , "  const uint8_t* p;"
                , "  if (condition) {"
                , "    p = message1;"
                , "  } else {"
                , "    p = message2;"
                , "  }"
                , "  network_send(p);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:17: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 2 [-Wsecurity-rank]"
                ]

        it "warns about tainted data from one of several return paths in a nested structure" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, return, 1)"
                , " * @security_rank(source, message, 0)"
                , " */"
                , "const uint8_t* process_message(const uint8_t *message, int status) {"
                , "  if (status == 0) {"
                , "    if (message != 0) {"
                , "      return message;"
                , "    }"
                , "    return (const uint8_t*)\"null message\";"
                , "  } else {"
                , "    return (const uint8_t*)\"ok\";"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:8: security risk: tainted data of rank Rank 0 returned from function with sink rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about complex tainted return" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , "/** @security_rank(source, message, 0) */"
                , "const uint8_t* get_message(const uint8_t *message, int status) {"
                , "  if (status > 0) {"
                , "    for (int i = 0; i < status; ++i) {"
                , "      if (i == 2) return message;"
                , "    }"
                , "  }"
                , "  return (const uint8_t*)\"safe\";"
                , "}"
                , ""
                , "/** @security_rank(source, msg, 0) */"
                , "void process_and_send(const uint8_t* msg, int status) {"
                , "  const uint8_t* m = get_message(msg, status);"
                , "  network_send(m);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:16: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted return from a function with multiple complex return paths" $ do
            ast <- mustParse
                [ "/**"
                , " * @security_rank(sink, return, 1)"
                , " * @security_rank(source, value, 0)"
                , " */"
                , "int get_value(int status, int value) {"
                , "  if (status == 0) {"
                , "    if (value > 100) {"
                , "      return 1;"
                , "    } else {"
                , "      return value;"
                , "    }"
                , "  } else if (status == 1) {"
                , "    for (int i = 0; i < 5; ++i) {"
                , "      if (i == 2) { return 3; }"
                , "    }"
                , "    return 4;"
                , "  }"
                , "  return 5;"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:10: security risk: tainted data of rank Rank 0 returned from function with sink rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data flow with pointer-to-pointer" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , "/** @security_rank(source, message, 0) */"
                , "void process(const uint8_t *message) {"
                , "  const uint8_t* tainted_ptr = message;"
                , "  const uint8_t** pptr = &tainted_ptr;"
                , "  network_send(*pptr);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:7: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data flow in a while loop with break and continue" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , "/** @security_rank(source, message, 0) */"
                , "void process(const uint8_t *message) {"
                , "  int i = 0;"
                , "  const uint8_t* p = (const uint8_t*)\"safe\";"
                , "  while(i < 10) {"
                , "    ++i;"
                , "    if (i < 5) {"
                , "      p = message;"
                , "      continue;"
                , "    }"
                , "    if (i == 7) {"
                , "      network_send(p);"
                , "      break;"
                , "    }"
                , "  }"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:14: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data flow with mutual recursion" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(int data);"
                , "/** @security_rank(source, x, 0) */"
                , "int f(int x);"
                , "int g(int x) {"
                , "  if (x > 0) return f(x - 1);"
                , "  return x;"
                , "}"
                , "int f(int x) {"
                , "  if (x > 0) return g(x - 1);"
                , "  return 20;"
                , "}"
                , "/** @security_rank(source, val, 0) */"
                , "void main(int val) {"
                , "  int res = f(val);"
                , "  network_send(res);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:16: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about taint propagated through multiple levels of struct nesting" $ do
            ast <- mustParse
                [ "struct Inner { const uint8_t* data; };"
                , "struct Middle { struct Inner inner; };"
                , "struct Outer { struct Middle middle; };"
                , "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , "/** @security_rank(source, message, 0) */"
                , "void process(const uint8_t *message) {"
                , "  struct Outer o;"
                , "  o.middle.inner.data = message;"
                , "  network_send(o.middle.inner.data);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:10: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "accepts safe data flow through multiple levels of struct nesting" $ do
            ast <- mustParse
                [ "struct Inner { const uint8_t* data; };"
                , "struct Middle { struct Inner inner; };"
                , "struct Outer { struct Middle middle; };"
                , "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , "/** @security_rank(source, message, 0) */"
                , "void process(const uint8_t *message) {"
                , "  struct Outer o;"
                , "  o.middle.inner.data = (const uint8_t*)\"safe\";"
                , "  network_send(o.middle.inner.data);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe` []

        it "warns when taint from one parameter affects another via a function call" $ do
            ast <- mustParse
                [ "void process_data(const uint8_t* in, uint8_t* out) {"
                , "  *out = *in;"
                , "}"
                , ""
                , "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , ""
                , "/** @security_rank(source, message, 0) */"
                , "void send(const uint8_t* message) {"
                , "  uint8_t buffer[128];"
                , "  process_data(message, buffer);"
                , "  network_send(buffer);"
                , "}"
                ]
            pendingWith "out summaries don't seem to work or propagate"
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:12: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data propagated through an array element" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , "/** @security_rank(source, message, 0) */"
                , "void process(const uint8_t *message) {"
                , "  uint8_t buffer[2];"
                , "  buffer[0] = message[0];"
                , "  network_send(&buffer[0]);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:7: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data propagated from a pointer to an array element" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , "/** @security_rank(source, p, 0) */"
                , "void process(const uint8_t *p) {"
                , "  uint8_t buffer[2];"
                , "  buffer[0] = *p;"
                , "  network_send(&buffer[0]);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:7: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data propagated from an array element to a pointer" $ do
            ast <- mustParse
                [ "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , "/** @security_rank(source, message, 0) */"
                , "void process(const uint8_t *message) {"
                , "  uint8_t buffer[2];"
                , "  buffer[0] = message[0];"
                , "  const uint8_t *p = &buffer[0];"
                , "  network_send(p);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:8: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data propagated through a union" $ do
            ast <- mustParse
                [ "typedef union MyUnion { const uint8_t* a; int b; } MyUnion;"
                , "/** @security_rank(sink, data, 1) */"
                , "void network_send(int data);"
                , "/** @security_rank(source, message, 0) */"
                , "void process(const uint8_t *message) {"
                , "  MyUnion u;"
                , "  u.a = message;"
                , "  network_send(u.b);"
                , "}"
                ]
            pendingWith "SecurityRank module needs to be implemented."
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:8: security risk: tainted data of rank 0 sent to sink of rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data propagated through a function pointer" $ do
            ast <- mustParse
                [ "typedef void sending_cb(const uint8_t* message);"
                , "/** @security_rank(sink, data, 1) */"
                , "void network_send(const uint8_t *data);"
                , "/** @security_rank(source, message, 0) */"
                , "void process(const uint8_t *message) {"
                , "  sending_cb *sender = network_send;"
                , "  sender(message);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:7: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]

        it "warns about tainted data propagated through an indirect call" $ do
            ast <- mustParse
                [ "typedef void my_cb(int x);"
                , "/** @security_rank(source, return, 0) */"
                , "int source();"
                , "/** @security_rank(sink, x, 1) */"
                , "void sink(int x);"
                , "void real_sink(int x) { sink(x); }"
                , "void test() {"
                , "  my_cb *fptr = &real_sink;"
                , "  int tainted = source();"
                , "  fptr(tainted);"
                , "}"
                ]
            let diags = analyseGlobal ["security-rank"] [("test.c", ast)]
            diags `shouldBe` ["test.c:10: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"]

    describe "points-to analysis dependent failures" $ do
        it "warns about taint propagated through a pointer across functions" $ do
            ast <- mustParse
                [ "/** @security_rank(source, p_out, 0) */"
                , "void get_tainted_ptr(char **p_out) {"
                , "  *p_out = (char*)\"tainted\";"
                , "}"
                , ""
                , "/** @security_rank(sink, data, 1) */"
                , "void network_send(char *data);"
                , ""
                , "void intermediary(char *p) {"
                , "  network_send(p);"
                , "}"
                , ""
                , "void main() {"
                , "  char *p = (char*)\"safe\";"
                , "  get_tainted_ptr(&p);"
                , "  intermediary(p);"
                , "}"
                ]
            analyseGlobal ["security-rank"] [("test.c", ast)]
                `shouldBe`
                [ "test.c:16: security risk: tainted data of rank Rank 0 sent to sink of rank Rank 1 [-Wsecurity-rank]"
                ]
