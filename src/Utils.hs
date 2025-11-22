module Utils where

import Linear
import Types
import Graphics.Gloss
import qualified Data.Map as Map
import qualified Data.Vector  as V

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

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate x y

getSpritePicture :: Map.Map String Sprite -> SpriteRef -> Picture
getSpritePicture smap (SpriteRef sr Nothing) = let
        sprite = smap Map.! sr
    in case sprite of
        Sprite _ (Left pic) -> pic
        Sprite _ (Right _) -> error "Animated sprite requires frame number"
getSpritePicture smap (SpriteRef sr (Just frameNum)) = let
        sprite = smap Map.! sr
    in case sprite of
        Sprite _ (Left _) -> error "Static sprite does not support frame number"
        Sprite _ (Right a) -> sprites a V.! frameNum

-- Transition easing
easeInOut :: Float -> Float
easeInOut t = t*t*(3 - 2*t)

lerp :: Float -> Float -> Float -> Float
lerp a b t = a + t * (b - a)