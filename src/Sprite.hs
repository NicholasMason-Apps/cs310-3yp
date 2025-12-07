{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sprite (loadSprite, drawSprite, spriteDimensions, stepAnimations, stepPositionFormula, checkBoundaryBoxIntersection, checkBoundaryBoxBottomIntersection, checkBoundaryBoxTopIntersection, checkBoundaryBoxLeftIntersection, checkBoundaryBoxRightIntersection) where

import Apecs
import Linear
import Control.Monad
import Types
import Data.Maybe ( isJust, fromMaybe )
import System.IO.Unsafe ( unsafePerformIO )
import SDL.Image (loadTexture)
import Codec.Picture
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as Set
import Utils
import Data.Map ((!))
import qualified SDL


spriteDimensions :: Sprite -> (Int, Int)
spriteDimensions (Sprite (w,h) _ Nothing) = (w, h)
spriteDimensions (Sprite (w,h) _ (Just a)) = (w `div` frameCount a, h)

stepAnimations :: Float -> System' ()
stepAnimations dT = do
    cmapM $ \(SpriteRef sr e) -> do
        Time t <- get global
        SpriteMap smap <- get global
        -- when (isJust e) $ liftIO $ print e
        let Sprite _ _ a = smap ! sr
        case a of
            Nothing -> return $ SpriteRef sr e
            Just a' -> let
                    trigger = floor (t / frameSpeed a') /= floor ((t + dT) / frameSpeed a')
                in return $ updateAnimation (SpriteRef sr e) trigger (frameCount a') (looping a') (afterLoopAnimation a')
    where
        updateAnimation :: SpriteRef -> Bool -> Int -> Bool -> Maybe String -> SpriteRef
        updateAnimation (SpriteRef sr Nothing) _ _ _ _ = SpriteRef sr Nothing
        updateAnimation (SpriteRef sr (Just a)) trigger fc l mstr =
            if trigger
            then let
                newFrame = (a + 1) `mod` fc
                in 
                    if l then 
                        SpriteRef sr (Just newFrame)
                    else 
                        case mstr of
                            Just str -> if newFrame == 0
                                        then SpriteRef str (Just 0)
                                        else SpriteRef sr (Just newFrame)
                            Nothing -> if a + 1 >= fc then
                                            SpriteRef sr (Just a)
                                        else SpriteRef sr (Just newFrame)
            else SpriteRef sr (Just a)

aabb :: V2 Float -> BoundaryBox -> (Float, Float, Float, Float)
aabb (V2 x y) (BoundaryBox (w,h) (ox,oy)) =
    let left   = x + fromIntegral ox
        top    = y + fromIntegral oy
        right  = left + fromIntegral w
        bottom = top + fromIntegral h
    in
        (left, top, right, bottom)

-- Boundary box collision detection
checkBoundaryBoxIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxIntersection v1 bb1 v2 bb2 = checkBoundaryBoxTopIntersection v1 bb1 v2 bb2 ||
                                            checkBoundaryBoxBottomIntersection v1 bb1 v2 bb2 ||
                                            checkBoundaryBoxLeftIntersection v1 bb1 v2 bb2 ||
                                            checkBoundaryBoxRightIntersection v1 bb1 v2 bb2
-- Note: Sprite positions are centered based on their Position component
checkBoundaryBoxTopIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxTopIntersection pos1 bb1 pos2 bb2 =
    let 
        (_, t1, _, b1) = aabb pos1 bb1
        (_, t2, _, _)  = aabb pos2 bb2
    in 
        b1 > t2 && t1 < t2

checkBoundaryBoxBottomIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxBottomIntersection pos1 bb1 pos2 bb2 =
    let 
        (_, t1, _, b1) = aabb pos1 bb1
        (_, _, _, b2)  = aabb pos2 bb2
    in 
        t1 < b2 && b1 > b2

checkBoundaryBoxLeftIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxLeftIntersection pos1 bb1 pos2 bb2 =
    let
        (l1, _, r1, _) = aabb pos1 bb1
        (l2, _, _, _)  = aabb pos2 bb2
    in
        r1 > l2 && l1 < l2

checkBoundaryBoxRightIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxRightIntersection pos1 bb1 pos2 bb2 =
    let 
        (l1, _, r1, _) = aabb pos1 bb1
        (_, _, r2, _)  = aabb pos2 bb2
    in 
        l1 < r2 && r1 > r2

loadSprite :: SDL.Renderer -> FilePath -> SDL.Texture
loadSprite r path = unsafePerformIO $ loadTexture r ("assets/" ++ path)

-- Draw a sprite given its SpriteRef and position
-- For sprite sheets, use the frame number to determine which part of the sheet to draw by applying a cropping rectangle
drawSprite :: SpriteRef -> SpriteMap -> Position -> SDL.Renderer -> IO ()
drawSprite (SpriteRef str Nothing) (SpriteMap smap) (Position pos) r = let
        (Sprite (w,h) t _) = smap ! str
        pos' = SDL.Rectangle (SDL.P (floor <$> pos)) (V2 (fromIntegral w) (fromIntegral h))
    in
        SDL.copy r t Nothing (Just pos')
drawSprite (SpriteRef str (Just frameNum)) (SpriteMap smap) (Position pos) r = let
        (Sprite (w,h) t ma) = smap ! str
        a = fromMaybe (error "Expected animation data for animated sprite") ma
        frameWidth = w `div` frameCount a
        dstRect = SDL.Rectangle (SDL.P (floor <$> pos)) (V2 (fromIntegral frameWidth) (fromIntegral h))
        srcRect = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral (frameNum * frameWidth)) 0)) (SDL.V2 (fromIntegral frameWidth) (fromIntegral h))
    in
        SDL.copy r t (Just srcRect) (Just dstRect)


-- loadStaticSprite :: Renderer -> FilePath -> Texture
-- loadStaticSprite r path = let
--     res = unsafePerformIO $ loadTexture r ("assets/" ++ path)
--     in case res of
--         Nothing -> error $ "Failed to load sprite: " ++ path
--         Just img -> img

-- loadAnimatedSprite :: Renderer -> FilePath -> Int -> (Int,Int) -> V.Vector Picture
-- loadAnimatedSprite r path frameCount (w,h) = let
--         res = unsafePerformIO $ readImage ("assets/" ++ path)
--         frameWidth = w `div` frameCount
--     in
--         case res of
--             Left err -> error $ "Failed to load sprite: " ++ path ++ " Error: " ++ err
--             Right dynImg -> let
--                     img = convertRGBA8 dynImg
--                     subImg i = generateImage (\x y -> pixelAt img (x + (i * frameWidth)) y) frameWidth h
--                 in
--                     V.generate frameCount (fromMaybe Blank . fromDynamicImage . ImageRGBA8 . subImg)