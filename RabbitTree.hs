module RabbitTree where

data RabbitHistory =
    Born RabbitHistory |
    Stayed RabbitHistory |
    Matured RabbitHistory |
    ImaginaryHistory Int
    deriving (Show, Eq, Ord)

imagineHistory :: RabbitHistory -> RabbitHistory
imagineHistory (ImaginaryHistory k) =
    Matured (Born (Stayed (ImaginaryHistory (k + 1))))

matured :: RabbitHistory -> RabbitHistory
matured (Born (Stayed (ImaginaryHistory k))) | k > 0 =
    ImaginaryHistory (k - 1)
matured h = Matured h

up :: RabbitHistory -> RabbitHistory
up (Born h) = h
up (Stayed h) = h
up (Matured h) = h
up h = up (imagineHistory h)

down :: RabbitHistory -> [RabbitHistory]
down (Born h) = [matured (Born h)]
down h = [Stayed h, Born h]

left :: RabbitHistory -> RabbitHistory
left (Born h)           = Stayed h             -- (a)
left (Matured (Born h)) = Born (Stayed h)      -- (b)
left (Stayed h)         = rightChild (left h)  -- (c, c')
    where rightChild h = last (down h)
left h = left (imagineHistory h)

right :: RabbitHistory -> RabbitHistory
right (Stayed h)        = Born h            -- (a)
right (Born (Stayed h)) = matured (Born h)  -- (b)
right (Born h)          = Stayed (right h)  -- (c')
right (Matured h)       = Stayed (right h)  -- (c)
right h = right (imagineHistory h)

isMature :: RabbitHistory -> Bool
isMature (Born _) = False
isMature _ = True
