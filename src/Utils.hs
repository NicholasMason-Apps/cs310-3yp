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