{-# LANGUAGE TemplateHaskell #-}

import Heptagrid

import Control.Monad
import Data.List
import Test.QuickCheck
import Test.QuickCheck.All()


instance Arbitrary Heptagon where
    arbitrary = oneof [
        return origin
        ,
        do adj <- liftM adjacent arbitrary
           elements adj
        ]


prop_has_seven_adjacent h =
    length (adjacent h) === 7

elementsUnique xs = conjoin [
    counterexample (show (xs, i, j)) (xs !! i /= xs !! j) |
    i <- [0..length xs - 1], j <- [0..i - 1]]

prop_adjacent_are_unique h =
    elementsUnique $ h : adjacent h

prop_adjacency_is_symmetric h =
    forAll (elements $ adjacent h) $ \h1 ->
        counterexample (show (h, h1)) (h `elem` adjacent h1)

nextElementInCyclicList xs x =
    let (Just i) = elemIndex x xs in
    xs !! ((i + 1) `mod` length xs)

-- Check that if h1 follows h2 in the list of h's adjacent tiles (CCW),
-- then h will follow h2 in h1's adjacent tiles:
--    h2
--   h  h1
prop_three_heptagons_meet h =
    let adj = adjacent h in
    forAll (elements $ adj) $ \h1 ->
        let h2 = nextElementInCyclicList adj h1
            adj1 = adjacent h1 in
        nextElementInCyclicList adj1 h2 === h


main = $forAllProperties $ \prop -> do
    result <-
        quickCheckWithResult stdArgs {maxSuccess = 100000, chatty=False} prop
    putStr $ output result
    return result
