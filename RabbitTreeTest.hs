{-# LANGUAGE TemplateHaskell #-}

import Control.Monad
import Test.QuickCheck
import Test.QuickCheck.All
import RabbitTree
instance Arbitrary RabbitHistory where
    arbitrary = oneof [
        return NoHistory,
        do h <- arbitrary
           elements $ down h
        ]

    shrink (Born h) = [h] ++ (map Born $ shrink h)
    shrink (Stayed h) = [h] ++ (map Stayed $ shrink h)
    shrink (Matured h) = [h] ++ (map Matured $ shrink h)
    shrink _ = []

prop_up_undoes_down h = and [up h1 == h | h1 <- down h]
prop_history_is_valid h = valid h

hasLeft NoHistory = False
hasLeft (Stayed h) = hasLeft h
hasLeft _ = True

prop_right_undoes_left h =
    hasLeft h ==>
    h == right (left h)

hasRight NoHistory = False
hasRight (Born h) = hasRight h
hasRight (Matured h) = hasRight h
hasRight _ = True

prop_left_undoes_right h =
    hasRight h ==>
    h == left (right h)

level NoHistory = 0
level (Born h) = 1 + level h
level (Stayed h) = 1 + level h
level (Matured h) = 1 + level h

prop_neighbors_are_at_the_same_level h =
    hasLeft h ==>
    level h == level (left h)

prop_neighbors_in_fifth_layer h =
    map left (tail fifth_layer) ==  take (length fifth_layer - 1) fifth_layer
    where
        fifth_layer = iterate ((down =<<) $) [h] !! 5


main = $quickCheckAll
