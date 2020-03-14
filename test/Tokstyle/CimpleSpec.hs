{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.CimpleSpec where

import           Test.Hspec             (Spec, describe, it, shouldBe)

import           Tokstyle.Cimple.AST    (LiteralType (..), Node (..),
                                         Scope (..))
import           Tokstyle.Cimple.Lexer  (AlexPosn (..), Lexeme (..), runAlex)
import           Tokstyle.Cimple.Parser (parseCimple)
import           Tokstyle.Cimple.Tokens (LexemeClass (..))


spec :: Spec
spec =
  describe "C parsing" $ do
    it "should parse a simple function" $ do
      let ast = runAlex "int a(void) { return 3; }" parseCimple
      ast `shouldBe` Right [
        FunctionDefn
          Global
          (FunctionPrototype
             (TyStd (L (AlexPn 0 1 1) IdStdType "int"))
             (L (AlexPn 4 1 5) IdVar "a")
             [])
          [Return (Just (LiteralExpr Int (L (AlexPn 21 1 22) LitInteger "3")))]]

    it "should parse a type declaration" $ do
      let ast = runAlex "typedef struct Foo { int x; } Foo;" parseCimple
      ast `shouldBe` Right [
        Typedef (
          Struct (L (AlexPn 15 1 16) IdSueType "Foo")
          [MemberDecl
            (TyStd (L (AlexPn 21 1 22) IdStdType "int"))
            (DeclSpecVar (L (AlexPn 25 1 26) IdVar "x"))
            Nothing])
        (L (AlexPn 30 1 31) IdSueType "Foo")]

    it "should parse a struct with bit fields" $ do
      let ast = runAlex "typedef struct Foo { int x : 123; } Foo;" parseCimple
      ast `shouldBe` Right [
        Typedef (
          Struct
            (L (AlexPn 15 1 16) IdSueType "Foo")
            [MemberDecl
              (TyStd (L (AlexPn 21 1 22) IdStdType "int"))
              (DeclSpecVar (L (AlexPn 25 1 26) IdVar "x"))
              (Just (L (AlexPn 29 1 30) LitInteger "123"))])
        (L (AlexPn 36 1 37) IdSueType "Foo")]

    it "should parse a comment" $ do
      let ast = runAlex "/* hello */" parseCimple
      ast `shouldBe` Right [
        Comment [CommentWord (L (AlexPn 3 1 4) CmtWord "hello")]]
