module Utils where

import Linear
import Types

screenWidth, screenHeight :: Int
screenWidth = 1280
screenHeight = 720

playerSpeed, bulletSpeed, enemySpeed, xmin, xmax :: Float
playerSpeed = 300
bulletSpeed = 500
enemySpeed  = 80
xmin = -640
xmax = 640

hitBonus, missPenalty :: Int
hitBonus = 100
missPenalty = 40

playerPos, scorePos :: V2 Float
playerPos = V2 0 0
scorePos  = V2 xmin (-170)

tileSize :: Num a => a
tileSize = 64

tileCount :: Integer
tileCount = 12

wallBottomCount :: Integer
wallBottomCount = 4

wallLeftCount :: Integer
wallLeftCount = 2

wallRightCount :: Integer
wallRightCount = 2

wallBottomLeftElbowCount :: Integer
wallBottomLeftElbowCount = 2

wallBottomRightElbowCount :: Integer
wallBottomRightElbowCount = 2

wallTopCount :: Integer
wallTopCount = 3

roomOffset :: Num a => a
roomOffset = 4 * tileSize

stepPositionFormula :: Float -> Position -> Velocity -> Position
stepPositionFormula dT (Position p) (Velocity v) = Position (p + dT *^ v)

-- Boundary box collision detection
-- Note: Sprite positions are centered based on their Position component
checkBoundaryBoxTopIntersection :: V2 Float -> Sprite -> V2 Float -> Sprite -> Bool
checkBoundaryBoxTopIntersection (V2 x1 y1) s1 (V2 x2 y2) s2 =
    bottom1 < top2 && top1 > top2 && right1 > left2 && left1 < right2
    where
        (w1,h1) = spriteDimensions s1
        (w2,h2) = spriteDimensions s2
        left1 = x1 - fromIntegral w1/2
        right1 = x1 + fromIntegral w1/2
        top1  = y1 + fromIntegral h1/2
        bottom1 = y1 - fromIntegral h1/2
        left2 = x2 - fromIntegral w2/2
        right2 = x2 + fromIntegral w2/2
        top2 = y2 + fromIntegral h2/2
checkBoundaryBoxBottomIntersection :: V2 Float -> Sprite -> V2 Float -> Sprite -> Bool
checkBoundaryBoxBottomIntersection (V2 x1 y1) s1 (V2 x2 y2) s2 =
    top1 > bottom2 && bottom1 < bottom2 && right1 > left2 && left1 < right2
    where
        (w1,h1) = spriteDimensions s1
        (w2,h2) = spriteDimensions s2
        left1 = x1 - fromIntegral w1/2
        right1 = x1 + fromIntegral w1/2
        top1  = y1 + fromIntegral h1/2
        bottom1 = y1 - fromIntegral h1/2
        left2 = x2 - fromIntegral w2/2
        right2 = x2 + fromIntegral w2/2
        bottom2 = y2 - fromIntegral h2/2
checkBoundaryBoxLeftIntersection :: V2 Float -> Sprite -> V2 Float -> Sprite -> Bool
checkBoundaryBoxLeftIntersection (V2 x1 y1) s1 (V2 x2 y2) s2 =
    right1 > left2 && left1 < left2 && bottom1 < top2 && top1 > bottom2
    where
        (w1,h1) = spriteDimensions s1
        (w2,h2) = spriteDimensions s2
        left1 = x1 - fromIntegral w1/2
        right1 = x1 + fromIntegral w1/2
        top1  = y1 + fromIntegral h1/2
        bottom1 = y1 - fromIntegral h1/2
        left2 = x2 - fromIntegral w2/2
        top2  = y2 + fromIntegral h2/2
        bottom2 = y2 - fromIntegral h2/2
checkBoundaryBoxRightIntersection :: V2 Float -> Sprite -> V2 Float -> Sprite -> Bool
checkBoundaryBoxRightIntersection (V2 x1 y1) s1 (V2 x2 y2) s2 =
    left1 < right2 && right1 > right2 && bottom1 < top2 && top1 > bottom2
    where
        (w1,h1) = spriteDimensions s1
        (w2,h2) = spriteDimensions s2
        left1 = x1 - fromIntegral w1/2
        right1 = x1 + fromIntegral w1/2
        top1  = y1 + fromIntegral h1/2
        bottom1 = y1 - fromIntegral h1/2
        right2 = x2 + fromIntegral w2/2
        top2  = y2 + fromIntegral h2/2
        bottom2 = y2 - fromIntegral h2/2

spriteDimensions :: Sprite -> (Int, Int)
spriteDimensions (Sprite (w,h) _) = (w, h)