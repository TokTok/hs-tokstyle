{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Linter.VarUnusedInScopeSpec where

import           Test.Hspec          (Spec, it, shouldBe)

import           Tokstyle.Linter     (allWarnings, analyseLocal)
import           Tokstyle.LinterSpec (mustParse)


spec :: Spec
spec = do
    it "detects vars declared outside an if-statement that could be declared inside it" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int foo = 0;"
            , "  if (true) {"
            , "    print_int(foo);"
            , "  }"
            , "}"
            ]
        analyseLocal ["var-unused-in-scope"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: variable `foo` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "ignores variables that escape the inner scope through pointers" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int foo;"
            , "  int *foo_ptr;"
            , "  if (true) {"
            , "    foo_ptr = &foo;"
            , "  }"
            , "  print_int(*foo_ptr);"
            , "}"
            ]
        analyseLocal ["var-unused-in-scope"] ("test.c", ast) `shouldBe` []

    it "ignores array-typed variables that escape the inner scope through assignment" $ do
        ast <- mustParse
            [ "int a(char *p) {"
            , "  char foo[3] = {0};"
            , "  if (p == nullptr) {"
            , "    p = foo;"
                    -- ^^^ We don't know that `foo` here is actually `&foo[0]`.
            , "  }"
            , "  print(p);"
            , "}"
            ]
        analyseLocal ["var-unused-in-scope"] ("test.c", ast) `shouldBe` []

    it "ignores array-typed variables assigned in a loop" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int foo[2] = {0, 0};"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    if (foo[0] == foo[1]) {"
            , "      print_int(foo[0]);"
            , "    }"
            , "    foo[0] = i % 2;"
            , "    foo[1] = i % 3;"
            , "  }"
            , "}"
            ]
        analyseLocal ["var-unused-in-scope"] ("test.c", ast) `shouldBe` []

    it "keeps conditional variable initialisation out of the `if` statement if it's used after" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int foo;"
            , "  if (true) {"
            , "    foo = 1;"
            , "  } else {"
            , "    foo = 2;"
            , "  }"
            , "  print_int(foo);"
            , "  for (int i = 0; i < foo; ++i) { /* nothing */ }"
            , "}"
            ]
        analyseLocal ["var-unused-in-scope"] ("test.c", ast) `shouldBe` []

    it "does not suggest moving complex initialisations into an if-statement" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int foo = maybe_side_effect();"
            , "  if (true) {"
            , "    print_int(foo);"
            , "  }"
            , "}"
            ]
        analyseLocal ["var-unused-in-scope"] ("test.c", ast) `shouldBe` []

    it "suggests moving trivial (pure) initialisations into an if-statement" $ do
        ast <- mustParse
            [ "int f(int a, int b) {"
            , "  int foo = 3 + 4;"
            , "  if (true) {"
            , "    print_int(foo);"
            , "  }"
            , "}"
            ]
        analyseLocal ["var-unused-in-scope"] ("test.c", ast)
            `shouldBe`
            [ "test.c:2: variable `foo` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "does not suggest moving loop variables into a `while` statement" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i = 0;"
            , "  while (true) {"
            , "    ++i;"
            , "    if (i > 10) {"
            , "      break;"
            , "    }"
            , "  }"
            , "}"
            ]
        analyseLocal ["var-unused-in-scope"] ("test.c", ast) `shouldBe` []

    it "detects decls that can be for-init-decls" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:3:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "detects reducible for-stmts followed by irreducible for-stmts" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { /* nothing */ }"
            , "  for (int i = 0; i < 10; ++i) { /* nothing */ }"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:3:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "leaves already correct for-init-decls alone" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  for (int i = 0; i < 10; ++i) { print_int(i); }"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe` []

    it "does not suggest reducing scope of loop bound constants" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  const int bound = 10;"
            , "  for (int i = 0; i < bound; ++i) { print_int(i); }"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe` []

    it "considers `&var` a write to `var`" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int foo;"
            , "  for (int i = start(&foo); !stop(foo); i = incr(&foo)) { print_ints(i, foo); }"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe` []

    it "ignores function parameters" $ do
        ast <- mustParse
            [ "int a(int i) {"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe` []

    it "supports #if/#endif" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "#if HAHA"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#endif /* HAHA */"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:3: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "supports #if/#else/#endif" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "#if HAHA"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#else"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#endif /* HAHA */"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:3: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "detects when the first #if branch is ok while the second isn't" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "#if HAHA"
            , "  for (int i = 0; i < 10; ++i) { /* nothing */ }"
            , "#else"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { /* nothing */ }"
            , "#endif /* HAHA */"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:5: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:6:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "detects when the second #if branch is ok while the first isn't" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "#if HAHA"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#else"
            , "  for (int i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "#endif /* HAHA */"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:3: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "detects multiple uses, as long as all of them are writes" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
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
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "detects variables only-written in both if branches" $ do
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
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:6:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "detects variables only-written in multiple (more than 2) if/else branches" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  if (true) {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  } else if (true) {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  } else {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  }"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast)
            `shouldBe`
            [ "test.c:2: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:8:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "should not suggest reducing scope if the variable is read in an if-condition" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  if (true) {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  } else if (i > 5) {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  } else {"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  }"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe` []

    it "allows vars read in the same scope" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); blah(); }"
            , "  print_int(i);"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe` []

    it "allows vars used as the bound for another for-loop" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  int i;"
            , "  for (i = 0; i < 10; ++i) { puts(\"hello!\"); blah(); }"
            , "  for (int j = 0; j < i; ++j) { puts(\"hello!\"); }"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe` []

    it "treats array member assignment as read" $ do
        ast <- mustParse
            [ "int a(char *p) {"
            , "  char *c = p;"
            , "  if (true) { c[0] = 'a'; }"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe` []

    it "treats dereference-and-assign as read" $ do
        ast <- mustParse
            [ "int a(char *p) {"
            , "  char *c = p;"
            , "  if (true) { *c = 'a'; }"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe` []

    it "should consider one `if` branch with a write as possibly not writing" $ do
        ast <- mustParse
            [ "int main(void) {"
            , "  int foo = 1;"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    if (i >= 5) {"
            , "      foo = 0;"
            , "    }"
            , "    printf(\"%d\\n\", foo);"
            , "  }"
            , "  return 0;"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe` []

    it "should combine 'either write or read' into 'just read'" $ do
        ast <- mustParse
            [ "int main(void) {"
            , "  int foo = 1;"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    if (i >= 5) {"
            , "      foo = 0;"
            , "    } else {"
            , "      print_int(foo);"
            , "    }"
            , "    print_int(foo);"
            , "  }"
            , "  return 0;"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe` []

    it "suggests reducing scope when all if-branches do writes" $ do
        ast <- mustParse
            [ "int main(void) {"
            , "  int foo = 1;"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    if (i >= 5) {"
            , "      foo = 0;"
            , "    } else if (i >= 3) {"
            , "      foo = 2;"
            , "    } else {"
            , "      foo = 3;"
            , "    }"
            , "    printf(\"%d\\n\", foo);"
            , "  }"
            , "  return 0;"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe`
            -- This suggestion is not quite correct, but we have nothing to anchor the "possibly to
            -- here" part to, so we make a slightly wrong suggestion. Hence the "possibly". In
            -- reality, the declaration should be right before the first `if` on line 4.
            [ "test.c:2: variable `foo` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:9:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "detects vars that can always be declared inside the for-loop" $ do
        ast <- mustParse
            [ "int main(void) {"
            , "  int foo = 1;"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    foo = 2;"  -- any writes to foo will be overwritten in the next loop iteration
            , "    if (i >= 5) {"
            , "      foo = 0;"
            , "    }"
            , "    printf(\"%d\\n\", foo);"
            , "  }"
            , "  return 0;"
            , "}"
            ]
        analyseLocal allWarnings ("test.c", ast) `shouldBe`
            [ "test.c:2: variable `foo` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:4:   possibly to here [-Wvar-unused-in-scope]"
            ]

    it "detects same-named variables where only one branch needs a diagnostic" $ do
        ast <- mustParse
            [ "int a(void) {"
            , "  if (true) {"
            , "    for (int i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  } else {"
            , "    int i;"
            , "    for (i = 0; i < 10; ++i) { puts(\"hello!\"); }"
            , "  }"
            , "}"
            ]
        analyseLocal ["var-unused-in-scope"] ("test.c", ast) `shouldBe`
            [ "test.c:5: variable `i` can be reduced in scope [-Wvar-unused-in-scope]"
            , "test.c:6:   possibly to here [-Wvar-unused-in-scope]"
            ]
