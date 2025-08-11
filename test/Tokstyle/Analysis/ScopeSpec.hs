{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Analysis.ScopeSpec (spec) where

import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Language.Cimple         as C
import           Language.Cimple.Pretty  (showNodePlain)
import           Test.Hspec
import           Tokstyle.Analysis.Scope
import           Tokstyle.LinterSpec     (mustParse)

spec :: Spec
spec = describe "Tokstyle.Analysis.Scope" $ do
    it "resolves a simple variable" $ do
        ast <- mustParse
            [ "int main() {"
            , "  int x;"
            , "  return x;"
            , "}"
            ]
        let (transformedAst, _finalState) = runScopePass ast
        let expected = Text.unlines
              [ "int main_1() {"
              , "  int x_2;"
              , ""
              , "  return x_2;"
              , "}"
              ]
        let actual = Text.unlines (map showNodePlain transformedAst)
        Text.stripEnd actual `shouldBe` Text.stripEnd expected

    it "handles name shadowing" $ do
        ast <- mustParse
            [ "int main() {"
            , "  int x;"
            , "  if (true) {"
            , "    int x;"
            , "    return x;"
            , "  }"
            , "  return x;"
            , "}"
            ]
        let (transformedAst, _finalState) = runScopePass ast
        let expected = Text.unlines
              [ "int main_1() {"
              , "  int x_2;"
              , ""
              , "  if (true) {"
              , "    int x_3;"
              , ""
              , "    return x_3;"
              , "  }"
              , ""
              , "  return x_2;"
              , "}"
              ]
        let actual = Text.unlines (map showNodePlain transformedAst)
        Text.stripEnd actual `shouldBe` Text.stripEnd expected

    it "handles function parameters" $ do
        ast <- mustParse
            [ "int f(int x) {"
            , "  return x;"
            , "}"
            ]
        let (transformedAst, _finalState) = runScopePass ast
        let expected = Text.unlines
              [ "int f_1(int x_2) {"
              , "  return x_2;"
              , "}"
              ]
        let actual = Text.unlines (map showNodePlain transformedAst)
        Text.stripEnd actual `shouldBe` Text.stripEnd expected

    it "handles global variables" $ do
        ast <- mustParse
            [ "const int x = 3;"
            , "int main() {"
            , "  return x;"
            , "}"
            ]
        let (transformedAst, _finalState) = runScopePass ast
        let expected = Text.unlines
              [ "const int x_1 = 3;"
              , "int main_2() {"
              , "  return x_1;"
              , "}"
              ]
        let actual = Text.unlines (map showNodePlain transformedAst)
        Text.stripEnd actual `shouldBe` Text.stripEnd expected

    it "handles static variables" $ do
        ast <- mustParse
            [ "static const int x = 3;"
            , "int main() {"
            , "  return x;"
            , "}"
            ]
        let (transformedAst, _finalState) = runScopePass ast
        let expected = Text.unlines
              [ "static const int x_1 = 3;"
              , "int main_2() {"
              , "  return x_1;"
              , "}"
              ]
        let actual = Text.unlines (map showNodePlain transformedAst)
        Text.stripEnd actual `shouldBe` Text.stripEnd expected

    it "handles function declarations and definitions" $ do
        ast <- mustParse
            [ "int f(int x);"
            , "int main() {"
            , "  return f(0);"
            , "}"
            , "int f(int x) {"
            , "  return x;"
            , "}"
            ]
        let (transformedAst, _finalState) = runScopePass ast
        let expected = Text.unlines
              [ "int f_1(int x_2);"
              , "int main_3() {"
              , "  return f_1(0);"
              , "}"
              , "int f_1(int x_4) {"
              , "  return x_4;"
              , "}"
              ]
        let actual = Text.unlines (map showNodePlain transformedAst)
        Text.stripEnd actual `shouldBe` Text.stripEnd expected

    it "handles for loop initializers" $ do
        ast <- mustParse
            [ "int main() {"
            , "  for (int i = 0; i < 10; ++i) {"
            , "    int x;"
            , "  }"
            , "}"
            ]
        let (transformedAst, _finalState) = runScopePass ast
        let expected = Text.unlines
              [ "int main_1() {"
              , "  for (int i_2 = 0; i_2 < 10; ++i_2) {"
              , "    int x_3;"
              , "  }"
              , "}"
              ]
        let actual = Text.unlines (map showNodePlain transformedAst)
        Text.stripEnd actual `shouldBe` Text.stripEnd expected

    it "handles structs" $ do
        ast <- mustParse
            [ "struct Struct {"
            , "  int x;"
            , "};"
            , "int main() {"
            , "  struct Struct s;"
            , "  s.x = 0;"
            , "  return s.x;"
            , "}"
            ]
        let (transformedAst, _finalState) = runScopePass ast
        let expected = Text.unlines
              [ "struct Struct {"
              , "  int x_1;"
              , "};"
              , "int main_2() {"
              , "  struct Struct s_3;"
              , ""
              , "  s_3.x = 0;"
              , ""
              , "  return s_3.x;"
              , "}"
              ]
        let actual = Text.unlines (map showNodePlain transformedAst)
        Text.stripEnd actual `shouldBe` Text.stripEnd expected

    it "handles unions" $ do
        ast <- mustParse
            [ "union Union {"
            , "  int x;"
            , "  float y;"
            , "};"
            , "int main() {"
            , "  union Union u;"
            , "  u.x = 0;"
            , "  return u.x;"
            , "}"
            ]
        let (transformedAst, _finalState) = runScopePass ast
        let expected = Text.unlines
              [ "union Union {"
              , "  int x_1;"
              , "  float y_2;"
              , "};"
              , "int main_3() {"
              , "  union Union u_4;"
              , ""
              , "  u_4.x = 0;"
              , ""
              , "  return u_4.x;"
              , "}"
              ]
        let actual = Text.unlines (map showNodePlain transformedAst)
        Text.stripEnd actual `shouldBe` Text.stripEnd expected

    it "handles enums" $ do
        ast <- mustParse
            [ "typedef enum Enum {"
            , "  ENUM_A,"
            , "  ENUM_B"
            , "} Enum;"
            , "int main() {"
            , "  Enum e = ENUM_A;"
            , "  return e;"
            , "}"
            ]
        let (transformedAst, _finalState) = runScopePass ast
        let expected = Text.unlines
              [ "typedef enum Enum {"
              , "  ENUM_A_1,"
              , "  ENUM_B_2,"
              , "} Enum;"
              , "int main_3() {"
              , "  Enum e_4 = ENUM_A_1;"
              , ""
              , "  return e_4;"
              , "}"
              ]
        let actual = Text.unlines (map showNodePlain transformedAst)
        Text.stripEnd actual `shouldBe` Text.stripEnd expected

    it "handles typedefs" $ do
        ast <- mustParse
            [ "typedef int My_Int;"
            , "int main() {"
            , "  My_Int x;"
            , "  return x;"
            , "}"
            ]
        let (transformedAst, _finalState) = runScopePass ast
        let expected = Text.unlines
              [ "typedef int My_Int;"
              , "int main_1() {"
              , "  My_Int x_2;"
              , ""
              , "  return x_2;"
              , "}"
              ]
        let actual = Text.unlines (map showNodePlain transformedAst)
        Text.stripEnd actual `shouldBe` Text.stripEnd expected

    it "handles a complex scenario" $ do
        ast <- mustParse
            [ "const int g = 3;"
            , "static const int s = 4;"
            , "int f(int p) {"
            , "  int l;"
            , "  return g + s + p + l;"
            , "}"
            , "int main() {"
            , "  return f(s);"
            , "}"
            ]
        let (transformedAst, _finalState) = runScopePass ast
        let expected = Text.unlines
              [ "const int g_1 = 3;"
              , "static const int s_2 = 4;"
              , "int f_3(int p_4) {"
              , "  int l_5;"
              , ""
              , "  return g_1 + s_2 + p_4 + l_5;"
              , "}"
              , "int main_6() {"
              , "  return f_3(s_2);"
              , "}"
              ]
        let actual = Text.unlines (map showNodePlain transformedAst)
        Text.stripEnd actual `shouldBe` Text.stripEnd expected

    it "handles a very complex scoping scenario" $ do
        ast <- mustParse
            [ "const int g = 1;"
            , "typedef int math_op_cb(int a, int b);"
            , "int add(int a, int b) { return a + b; }"
            , "int complex_scope(int g) {"
            , "  int s = 0;"
            , "  s = s + 1;"
            , "  math_op_cb *operation = &add;"
            , "  if (s > 1) {"
            , "    int s = 100;"
            , "    return operation(g, s);"
            , "  }"
            , "  return operation(g, s);"
            , "}"
            ]
        let (transformedAst, finalState) = runScopePass ast
        let expected = Text.unlines
              [ "const int g_1 = 1;"
              , "typedef int math_op_cb(int a_2, int b_3);"
              , "int add_4(int a_5, int b_6) {"
              , "  return a_5 + b_6;"
              , "}"
              , "int complex_scope_7(int g_8) {"
              , "  int s_9 = 0;"
              , ""
              , "  s_9 = s_9 + 1;"
              , ""
              , "  math_op_cb* operation_10 = &add_4;"
              , ""
              , "  if (s_9 > 1) {"
              , "    int s_11 = 100;"
              , ""
              , "    return operation_10(g_8, s_11);"
              , "  }"
              , ""
              , "  return operation_10(g_8, s_9);"
              , "}"
              ]
        let actual = Text.unlines (map showNodePlain transformedAst)
        ssErrors finalState `shouldBe` []
        Text.stripEnd actual `shouldBe` Text.stripEnd expected

    it "reports an error for variables used out of scope" $ do
        ast <- mustParse
            [ "int main() {"
            , "  for (int i = 0; i < 1; ++i) { continue; }"
            , "  return i;"
            , "}"
            ]
        let (_transformedAst, finalState) = runScopePass ast
        ssErrors finalState `shouldBe` ["Undeclared variable: \"i\""]
