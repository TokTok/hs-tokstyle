{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Analysis.ContextSpec where

import           Test.Hspec
import           Tokstyle.Analysis.Context (pushContext)

spec :: Spec
spec = do
    describe "pushContext" $ do
        it "pushes to an empty context" $ do
            pushContext 2 101 [] `shouldBe` [101]

        it "pushes to a non-full context" $ do
            pushContext 2 102 [101] `shouldBe` [102, 101]

        it "pushes to a full context, truncating the oldest element" $ do
            pushContext 2 103 [102, 101] `shouldBe` [103, 102]

        it "handles a k-limit of 1" $ do
            pushContext 1 102 [101] `shouldBe` [102]

        it "handles a k-limit of 0" $ do
            pushContext 0 101 [] `shouldBe` []
            pushContext 0 102 [101] `shouldBe` []

        it "returns the same context if the new element is prepended and then truncated" $ do
            let context' = [102, 101]
            pushContext 2 103 context' `shouldBe` [103, 102]
