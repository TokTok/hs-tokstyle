{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.C.LinterSpec
    ( mustParse
    , spec
    ) where

import           Test.Hspec                  (Spec, it, shouldBe)

import qualified Data.Text                   as Text
import           Language.C                  (CTranslUnit)
import           Language.C.Data.InputStream (inputStreamFromString)
import           Language.C.Data.Position    (Position, position)
import           Language.C.Parser           (parseC)
import           Tokstyle.C.Linter           (allWarnings, analyse)


startPos :: Position
startPos = position 0 "test.c" 1 0 Nothing


mustParse :: MonadFail m => [String] -> m CTranslUnit
mustParse code =
    let is = inputStreamFromString $ unlines code in
    case parseC is startPos of
        Left err -> fail $ show err
        Right ok -> return ok


spec :: Spec
spec = do
    it "should parse a simple function" $ do
        ast <- mustParse ["int a(void) { return 3; }"]
        analyse allWarnings ast `shouldBe` []

    it "should give diagnostics on invalid symbol redeclaration" $ do
        ast <- mustParse
            [ "typedef struct Foo { char x; } Foo;"
            , "typedef enum Foo { FOO_ONE } Foo;"
            ]
        analyse allWarnings ast
            `shouldBe`
            [ Text.unlines
                [ "test.c:2: (column 9) [ERROR]  >>> Foo redefined"
                , "  Foo previously declared as a different kind of symbol"
                , "  The previous declaration was here: "
                , "  (\"test.c\": line 1)"
                ]
            ]
