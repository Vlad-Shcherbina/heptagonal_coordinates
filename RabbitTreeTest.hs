{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.All
import RabbitTree

instance Arbitrary RabbitHistory where
    -- Only generates valid histories.
    arbitrary = oneof [
        liftM ImaginaryHistory (elements [0..5]),
        do h <- arbitrary
           elements $ down h
        ]

    -- Shrinking results should be valid histories.
    shrink (Born h) = map Born $ shrink h
    shrink (Stayed h) = [h] ++ (map Stayed $ shrink h)
    shrink (Matured h) = map matured $ shrink h
    shrink (ImaginaryHistory k) | k > 0 = [ImaginaryHistory (k - 1)]
    shrink _ = []


prop_history_is_valid h = valid h

prop_up_undoes_down h = and [up h1 == h | h1 <- down h]

prop_down_undoes_up h = h `elem` down (up h)

prop_right_undoes_left h =
    h == right (left h)

prop_left_undoes_right h =
    h == left (right h)

level (ImaginaryHistory k) = - 3 * k
level (Born h) = 1 + level h
level (Stayed h) = 1 + level h
level (Matured h) = 1 + level h

prop_neighbors_are_at_the_same_level h =
    level h == level (left h)

prop_neighbors_in_fifth_layer h =
    map left (tail fifth_layer) ==  take (length fifth_layer - 1) fifth_layer
    where
        fifth_layer = iterate ((down =<<) $) [h] !! 5


main = $quickCheckAll
