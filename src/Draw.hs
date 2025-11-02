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

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate x y

triangle, diamond :: Picture
triangle = Line [(0,0),(-0.5,-1),(0.5,-1),(0,0)]
diamond  = Line [(-1,0),(0,-1),(1,0),(0,1),(-1,0)]

getSpritePicture :: Sprite -> Picture
getSpritePicture (Sprite img _ Nothing) = fromMaybe Blank (fromDynamicImage (ImageRGBA8 img))
getSpritePicture (Sprite sheet (w,h) (Just a)) = let
        subImg = generateImage (\x y -> pixelAt sheet (x + (fromIntegral (currentFrame a - 1) * (w `div` frameCount a))) y) (w `div` frameCount a) h
    in fromMaybe Blank (fromDynamicImage (ImageRGBA8 subImg))

draw :: System' Picture
draw = do
    player <- foldDraw $ \(Player, pos, s) -> translate' pos $ getSpritePicture s
    -- targets <- foldDraw $ \(Target, pos) -> translate' pos $ color red $ scale 10 10 diamond
    -- bullets <- foldDraw $ \(Bullet, pos) -> translate' pos $ color yellow $ scale 4 4 diamond
    particles <- foldDraw $ \(Particle _, Velocity (V2 vx vy), pos) ->
        translate' pos $ color orange $ Line [(0,0),(vx/10, vy/10)]
    walls <- foldDraw $ \(Wall, pos, s) -> translate' pos $ getSpritePicture s
    tiles <- foldDraw $ \(Tile, pos, s) -> translate' pos $ getSpritePicture s
    playerPos <- cfold (\_ (Player, Position p) -> Just p) Nothing
    -- TODO: map drawing
    let playerPosText = case playerPos of
            Just (V2 x y) -> color white $ translate' (Position (V2 (x-50) (y+20))) $ scale 0.1 0.1 $ Text $ "(" ++ show (round x) ++ "," ++ show (round y) ++ ")"
            Nothing       -> Blank
    let world = player <> particles <> playerPosText <> walls <> tiles
    let camera = case playerPos of
            Just (V2 x y) -> translate (-x) (-y) world
            Nothing       -> world
    return camera