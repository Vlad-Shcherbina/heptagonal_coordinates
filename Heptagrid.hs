module Heptagrid (Heptagon, origin, adjacent) where

import RabbitTree

data Heptagon = Heptagon RabbitHistory
    deriving (Eq, Show)

origin :: Heptagon
origin = Heptagon (ImaginaryHistory 0)

adjacent :: Heptagon -> [Heptagon]
adjacent (Heptagon h) = map Heptagon (
    let
        grandparent = up (up h)
        left_cousin = left h
        right_cousin = right h
        grandchildren = [h2 | h1 <- down h, h2 <- down h1]
    in
        [grandparent] ++
        (if not (isMature left_cousin) &&
            not (isMature right_cousin)
            then [left grandparent]            -- 101
            else []) ++
        [left_cousin] ++
        (if isMature left_cousin
            then [left (head grandchildren)]   -- 010, 001
            else []) ++
        grandchildren ++
        (if isMature left_cousin &&
            isMature right_cousin
            then [right (last grandchildren)]  -- 010
            else []) ++
        [right_cousin] ++
        (if not (isMature left_cousin) &&
            isMature right_cousin
            then [right grandparent]           -- 100
            else [])
    )
