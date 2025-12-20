{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Utils where

import Linear
import Types
import Graphics.Gloss
import qualified Data.Map as Map
import qualified Data.Vector  as V
import Apecs
import Control.Monad
import Data.Maybe ( fromMaybe )

playerSpeed, bulletSpeed, enemySpeed, xmin, xmax :: Float
playerSpeed = 250
bulletSpeed = 500
enemySpeed  = 275
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
        Sprite _ (GlossRenderer (Left pic)) -> pic
        Sprite _ (SDLRenderer _) -> Blank
        Sprite _ (GlossRenderer (Right _)) -> Blank
getSpritePicture smap (SpriteRef sr (Just frameNum)) = let
        sprite = smap Map.! sr
    in case sprite of
        Sprite _ (GlossRenderer (Left _)) -> Blank
        Sprite _ (SDLRenderer _) -> Blank
        Sprite _ (GlossRenderer (Right a)) -> if frameNum >= frameCount a then
                fromMaybe V.empty (sprites a) V.! (frameCount a - 1)
            else
                fromMaybe V.empty (sprites a) V.! frameNum

-- Transition easing
easeInOut :: Float -> Float
easeInOut t = t*t*(3 - 2*t)

lerp :: Float -> Float -> Float -> Float
lerp a b t = a + t * (b - a)

startTransition :: Float -> Float -> System' ()
startTransition angle speed = do
    cmapM_ $ \(Transition _ _ _ _, e) -> destroy e (Proxy @Transition)
    void $ newEntity (Transition { trProgress = 0, trAngle = angle, trSpeed = speed, trCoverEventFired = False })

-- Update positions based on velocity and delta time
stepPosition :: Float -> System' ()
stepPosition dT = cmap $ uncurry (stepPositionFormula dT)

spawnParticle :: Position -> Position -> String -> Int -> System' Entity
spawnParticle startPos endPos sref frameOffset = do
    SpriteMap smap <- get global
    let Sprite _ rs = smap Map.! sref
        frameCount' = case rs of
            GlossRenderer (Left _) -> 1
            SDLRenderer (_, Nothing) -> 1
            GlossRenderer (Right a) -> frameCount a
            SDLRenderer (_, Just a) -> frameCount a
        frameSpeed' = case rs of
            GlossRenderer (Left _) -> 0.1
            SDLRenderer (_, Nothing) -> 0.1
            GlossRenderer (Right a) -> frameSpeed a
            SDLRenderer (_, Just a) -> frameSpeed a
        (Position start) = startPos
        (Position end) = endPos
        vel = (end - start) ^/ ((fromIntegral frameCount' - fromIntegral frameOffset) * frameSpeed')
    newEntity (Particle endPos, startPos, Velocity vel, SpriteRef sref (Just 0))