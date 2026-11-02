{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.DestructorsSpec (spec) where

import           Data.Fix                             (Fix (..))
import           Data.Text                            (Text)
import           Language.Cimple                      (Lexeme (..), NodeF (..))
import           Test.Hspec                           (Spec, describe, it,
                                                       shouldBe)
import           Tokstyle.Cimple.Analysis.Destructors (destroyed, destructors)
import           Tokstyle.Cimple.Analysis.Functions   (Function (..),
                                                       definitions, universe)
import           Tokstyle.LinterSpec                  (mustParse)


destroyedIn :: [Text] -> IO [Text]
destroyedIn code = do
    nodes <- mustParse code
    let tus = [("test.c", nodes)]
        ds = destructors tus
    return
        [ v
        | fn <- definitions tus, fnName fn == "caller"
        , call@(Fix FunctionCall{}) <- universe (fnDefn fn)
        , Fix (VarExpr (L _ _ v)) <- destroyed ds call ]


spec :: Spec
spec = describe "destructors" $ do
    it "does not count a parameter as destroyed where another is found null" $ do
        vs <- destroyedIn
            [ "void chat_close(Chat *chat, Log *log) {"
            , "  if (log == nullptr) {"
            , "    return;"
            , "  }"
            , "  free(chat);"
            , "}"
            , "void caller(Chat *chat, Log *log) {"
            , "  chat_close(chat, log);"
            , "}"
            ]
        vs `shouldBe` []

    it "counts a field as destroyed when only a count in it is set afterwards" $ do
        vs <- destroyedIn
            [ "void mods_clear(Mods *mods) {"
            , "  free(mods->list);"
            , "}"
            , "void chat_reset(Chat *chat) {"
            , "  mods_clear(&chat->mods);"
            , "  chat->mods.count = 0;"
            , "}"
            , "void caller(Chat *chat) {"
            , "  chat_reset(chat);"
            , "}"
            ]
        vs `shouldBe` ["chat"]
