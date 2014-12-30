module RabbitTree where

data RabbitHistory =
    Born RabbitHistory |
    Stayed RabbitHistory |
    Matured RabbitHistory |
    ImaginaryHistory Int
    deriving (Show, Eq)

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
isMature (Born h) = False
isMature _ = True

valid :: RabbitHistory -> Bool
valid (ImaginaryHistory k) = k >= 0
valid (Born h) = isMature h && valid h
valid (Stayed h) = isMature h && valid h
valid (Matured h) = not (isMature h) && valid h && matured h == Matured h
