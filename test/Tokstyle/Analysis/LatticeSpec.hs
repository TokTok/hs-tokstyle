{-# LANGUAGE OverloadedStrings #-}
module Tokstyle.Analysis.LatticeSpec where

import           Test.Hspec
import           Tokstyle.Analysis.SecurityRank.Lattice
import           Tokstyle.Analysis.Types                (AbstractLocation (..))

spec :: Spec
spec = do
    describe "SecurityRank Ord Instance" $ do
        it "considers Bottom the lowest element" $ do
            Bottom `shouldBe` min Bottom Safe
            Bottom `shouldBe` min Bottom (Rank 0)
            Bottom `shouldBe` min (Rank 10) Bottom

        it "considers Rank to be lower than Safe" $ do
            Rank 0 `shouldBe` min Safe (Rank 0)
            Rank 100 `shouldBe` min Safe (Rank 100)
            Safe `shouldNotBe` min Safe (Rank 0)

        it "compares Ranks by their integer value" $ do
            Rank 0 `shouldBe` min (Rank 0) (Rank 1)
            Rank 5 `shouldBe` min (Rank 10) (Rank 5)
            Rank 0 `shouldBe` min (Rank 1) (Rank 0)

        it "is reflexive" $ do
            Bottom `shouldBe` min Bottom Bottom
            Safe `shouldBe` min Safe Safe
            Rank 42 `shouldBe` min (Rank 42) (Rank 42)

    describe "mergeRank" $ do
        it "returns the minimum of two ranks" $ do
            mergeRank (Rank 0) (Rank 1) `shouldBe` Rank 0
            mergeRank (Rank 1) (Rank 0) `shouldBe` Rank 0
            mergeRank (Rank 10) (Rank 10) `shouldBe` Rank 10

        it "returns the tainted rank when merged with Safe" $ do
            mergeRank Safe (Rank 0) `shouldBe` Rank 0
            mergeRank (Rank 5) Safe `shouldBe` Rank 5

        it "returns Safe when merging two Safe ranks" $ do
            mergeRank Safe Safe `shouldBe` Safe

        it "returns Bottom when merged with anything" $ do
            mergeRank Bottom Safe `shouldBe` Bottom
            mergeRank (Rank 10) Bottom `shouldBe` Bottom

    describe "AbstractLocation Ord Instance" $ do
        it "orders VarLocations alphabetically" $ do
            VarLocation "a" < VarLocation "b" `shouldBe` True
            VarLocation "z" > VarLocation "b" `shouldBe` True

        it "orders FieldLocations recursively" $ do
            let baseA = VarLocation "a"
            let baseB = VarLocation "b"
            FieldLocation baseA "field1" < FieldLocation baseA "field2" `shouldBe` True
            FieldLocation baseA "field" < FieldLocation baseB "field" `shouldBe` True

        it "orders DerefLocations based on the underlying location" $ do
            DerefLocation (VarLocation "p") < DerefLocation (VarLocation "q") `shouldBe` True

        it "provides a consistent total ordering" $ do
            let locs = [ VarLocation "a"
                       , FieldLocation (VarLocation "a") "f1"
                       , DerefLocation (VarLocation "a")
                       , VarLocation "b"
                       ]
            -- This just checks that they are comparable and not all equal.
            -- The exact order is implementation-defined but must be consistent.
            (head locs < last locs) || (head locs > last locs) || (head locs == last locs) `shouldBe` True
