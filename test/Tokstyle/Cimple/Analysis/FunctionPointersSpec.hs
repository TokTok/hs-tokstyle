{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Cimple.Analysis.FunctionPointersSpec (spec) where

import qualified Data.Map.Strict                           as Map
import qualified Data.Set                                  as Set
import           Data.Text                                 (Text)
import           Test.Hspec                                (Spec, describe, it,
                                                            shouldBe)
import           Tokstyle.Cimple.Analysis.FunctionPointers (Targets (..),
                                                            fieldTargets)
import           Tokstyle.LinterSpec                       (mustParse)


targetsOf :: [(FilePath, [Text])] -> (Text, Text) -> IO (Maybe Targets)
targetsOf files key = do
    tus <- mapM (\(file, code) -> (,) file <$> mustParse code) files
    return (Map.lookup key (fieldTargets tus))


header :: [Text]
header =
    [ "typedef void kill_cb(void *obj);"
    , "typedef struct Funcs {"
    , "  kill_cb *kill;"
    , "} Funcs;"
    ]


spec :: Spec
spec = describe "fieldTargets" $ do
    it "finds the one function a struct is initialised with" $ do
        t <- targetsOf
            [ ("funcs.h", header)
            , ("funcs.c",
                [ "static void os_kill(void *obj) {"
                , "  free(obj);"
                , "}"
                , "static const Funcs os_funcs = {"
                , "  os_kill,"
                , "};"
                ])
            ]
            ("Funcs", "kill")
        t `shouldBe` Just (Only (Set.fromList ["os_kill"]))

    it "follows functions through the parameters of the functions storing them" $ do
        t <- targetsOf
            [ ("funcs.h", header)
            , ("funcs.c",
                [ "static void set_kill(Funcs *funcs, kill_cb *kill) {"
                , "  funcs->kill = kill;"
                , "}"
                , "static void a_kill(void *obj) {"
                , "  free(obj);"
                , "}"
                , "static void b_kill(void *obj) {"
                , "  free(obj);"
                , "}"
                , "void setup(Funcs *a, Funcs *b) {"
                , "  set_kill(a, a_kill);"
                , "  set_kill(b, &b_kill);"
                , "}"
                ])
            ]
            ("Funcs", "kill")
        t `shouldBe` Just (Only (Set.fromList ["a_kill", "b_kill"]))

    it "says a field may hold anything a client can give it" $ do
        t <- targetsOf
            [ ("funcs.h", header)
            , ("tox.h", ["void tox_set_kill(Funcs *funcs, kill_cb *kill);"])
            , ("funcs.c",
                [ "void tox_set_kill(Funcs *funcs, kill_cb *kill) {"
                , "  funcs->kill = kill;"
                , "}"
                ])
            ]
            ("Funcs", "kill")
        t `shouldBe` Just (Anything Set.empty)

    it "says a field given a local may hold anything, even one named like a function" $ do
        t <- targetsOf
            [ ("funcs.h", header)
            , ("funcs.c",
                [ "static void a_kill(void *obj) {"
                , "  free(obj);"
                , "}"
                , "void setup(Funcs *funcs) {"
                , "  kill_cb *a_kill = next_kill();"
                , "  funcs->kill = a_kill;"
                , "}"
                ])
            ]
            ("Funcs", "kill")
        t `shouldBe` Just (Anything Set.empty)
