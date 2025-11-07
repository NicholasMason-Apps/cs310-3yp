{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Draw ( draw ) where

import Apecs
import Apecs.Gloss
import Linear
import Types
import Codec.Picture
import Graphics.Gloss.Juicy (fromDynamicImage)
import Data.Maybe
import qualified Data.Vector as V

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate x y

triangle, diamond :: Picture
triangle = Line [(0,0),(-0.5,-1),(0.5,-1),(0,0)]
diamond  = Line [(-1,0),(0,-1),(1,0),(0,1),(-1,0)]

getSpritePicture :: Sprite -> Picture
getSpritePicture (Sprite _ (Left p)) = p
getSpritePicture (Sprite _ (Right a)) = sprites a V.! (currentFrame a - 1)

isSpriteInView :: Maybe (V2 Float) -> Sprite -> Position -> Bool
isSpriteInView (Just (V2 px py)) (Sprite (w,h) p) (Position (V2 sx sy)) =
    let
        w' = case p of
            Left _ -> w
            Right a -> w `div` frameCount a
        vw = 1280
        vh = 720
        inViewTop = py + vh / 2 >= sy - fromIntegral h / 2
        inViewBottom = py - vh / 2 <= sy + fromIntegral h / 2
        inViewLeft = px - vw / 2 <= sx + fromIntegral w' / 2
        inViewRight = px + vw / 2 >= sx - fromIntegral w' / 2
    in
        inViewTop && inViewBottom && inViewLeft && inViewRight
isSpriteInView Nothing _ _ = True

draw :: System' Picture
draw = do
    playerPos <- cfold (\_ (Player, Position p) -> Just p) Nothing
    player <- foldDraw $ \(Player, pos, s) -> translate' pos $ getSpritePicture s
    -- targets <- foldDraw $ \(Target, pos) -> translate' pos $ color red $ scale 10 10 diamond
    bullets <- foldDraw $ \(Bullet, pos) -> translate' pos $ color yellow $ scale 4 4 diamond
    particles <- foldDraw $ \(Particle _, Velocity (V2 vx vy), pos) ->
        translate' pos $ color orange $ Line [(0,0),(vx/10, vy/10)]
    walls <- foldDraw $ \(Wall, pos, s) -> if isSpriteInView playerPos s pos
        then translate' pos $ getSpritePicture s
        else Blank
    tiles <- foldDraw $ \(Tile, pos, s) -> if isSpriteInView playerPos s pos
        then translate' pos $ getSpritePicture s
        else Blank
    let playerPosText = case playerPos of
            Just (V2 x y) -> color white $ translate' (Position (V2 (x-50) (y+20))) $ scale 0.1 0.1 $ Text $ "(" ++ show (round x) ++ "," ++ show (round y) ++ ")"
            Nothing       -> Blank
    let world = walls <> tiles <> bullets <> player <> playerPosText <> particles
    let camera = case playerPos of
            Just (V2 x y) -> translate (-x) (-y) world
            Nothing       -> world
    return camera