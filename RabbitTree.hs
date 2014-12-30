module RabbitTree where

data RabbitHistory =
    Born RabbitHistory |
    Stayed RabbitHistory |
    Matured RabbitHistory |
    NoHistory
    deriving (Show, Eq)

up :: RabbitHistory -> RabbitHistory
up (Born h) = h
up (Stayed h) = h
up (Matured h) = h

down :: RabbitHistory -> [RabbitHistory]
down (Born h) = [Matured (Born h)]
down h = [Stayed h, Born h]

left :: RabbitHistory -> RabbitHistory
left (Born h)           = Stayed h         -- (a)
left (Matured (Born h)) = Born (Stayed h)  -- (b)
left (Stayed h)         =
    case left h of
        Matured h1 -> Born (Matured h1)    -- (c)
        Born h1    -> Matured (Born h1)    -- (c')

right :: RabbitHistory -> RabbitHistory
right (Stayed h)        = Born h            -- (a)
right (Born (Stayed h)) = Matured (Born h)  -- (b)
right (Born h)          = Stayed (right h)  -- (c')
right (Matured h)       = Stayed (right h)  -- (c)

isMature :: RabbitHistory -> Bool
isMature (Born h) = False
isMature _ = True

valid :: RabbitHistory -> Bool
valid NoHistory = True
valid (Born h) = isMature h && valid h
valid (Stayed h) = isMature h && valid h
valid (Matured h) = not (isMature h) && valid h
