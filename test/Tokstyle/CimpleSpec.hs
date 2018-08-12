module Tokstyle.CimpleSpec where

import           Test.Hspec

import           Tokstyle.Cimple.Lexer  (runAlex)
import           Tokstyle.Cimple.Parser (parseCimple)


spec :: Spec
spec =
  describe "C parsing" $ do
    it "should parse a simple function" $ do
      let ast = runAlex "int a(void) { return 3; }" parseCimple
      ast `shouldBe` Right [()]

    it "should parse a type declaration" $ do
      let ast = runAlex "typedef struct Foo { int x; } Foo;" parseCimple
      ast `shouldBe` Right [()]
