{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sprite (spriteDimensions, loadStaticSprite, loadAnimatedSprite, stepAnimations, stepPositionFormula, checkBoundaryBoxIntersection, checkBoundaryBoxBottomIntersection, checkBoundaryBoxTopIntersection, checkBoundaryBoxLeftIntersection, checkBoundaryBoxRightIntersection) where

import Apecs
import Linear
import Control.Monad
import Types
import Data.Maybe ( isJust, fromMaybe )
import System.IO.Unsafe ( unsafePerformIO )
import Graphics.Gloss ( Picture (Blank) )
import Graphics.Gloss.Juicy ( loadJuicy, fromDynamicImage )
import Codec.Picture
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as Set
import Utils
import Data.Map ((!))

spriteDimensions :: Sprite -> (Int, Int)
spriteDimensions (Sprite (w,h) _) = (w, h)

stepAnimations :: Float -> System' ()
stepAnimations dT = do
    cmapM $ \(SpriteRef sr e) -> do
        Time t <- get global
        SpriteMap smap <- get global
        let Sprite _ spriteE = smap ! sr
        case spriteE of
            GlossRenderer (Left _) -> return $ SpriteRef sr e
            SDLRenderer (_, Nothing) -> return $ SpriteRef sr e
            GlossRenderer (Right a) -> let
                    trigger = floor (t / frameSpeed a) /= floor ((t + dT) / frameSpeed a)
                in return $ updateAnimation (SpriteRef sr e) trigger (frameCount a) (looping a) (afterLoopAnimation a)
            SDLRenderer (_, Just a) -> let
                    trigger = floor (t / frameSpeed a) /= floor ((t + dT) / frameSpeed a)
                in return $ updateAnimation (SpriteRef sr e) trigger (frameCount a) (looping a) (afterLoopAnimation a)
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

-- Boundary box collision detection
checkBoundaryBoxIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxIntersection v1 bb1 v2 bb2 = checkBoundaryBoxTopIntersection v1 bb1 v2 bb2 ||
                                            checkBoundaryBoxBottomIntersection v1 bb1 v2 bb2 ||
                                            checkBoundaryBoxLeftIntersection v1 bb1 v2 bb2 ||
                                            checkBoundaryBoxRightIntersection v1 bb1 v2 bb2
-- Note: Sprite positions are centered based on their Position component
checkBoundaryBoxTopIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxTopIntersection (V2 x1 y1) (BoundaryBox (w1, h1) (box1, boy1)) (V2 x2 y2) (BoundaryBox (w2, h2) (box2, boy2)) =
    bottom1 < top2 && top1 > top2 && right1 > left2 && left1 < right2
    where
        left1 = x1 + fromIntegral box1 - fromIntegral w1/2
        right1 = x1 + fromIntegral box1 + fromIntegral w1/2
        top1  = y1 + fromIntegral boy1 + fromIntegral h1/2
        bottom1 = y1 + fromIntegral boy1 - fromIntegral h1/2
        left2 = x2 + fromIntegral box2 - fromIntegral w2/2
        right2 = x2 + fromIntegral box2 + fromIntegral w2/2
        top2 = y2 + fromIntegral boy2 + fromIntegral h2/2
checkBoundaryBoxBottomIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxBottomIntersection (V2 x1 y1) (BoundaryBox (w1, h1) (box1, boy1)) (V2 x2 y2) (BoundaryBox (w2, h2) (box2, boy2)) =
    top1 > bottom2 && bottom1 < bottom2 && right1 > left2 && left1 < right2
    where
        left1 = x1 + fromIntegral box1 - fromIntegral w1/2
        right1 = x1 + fromIntegral box1 + fromIntegral w1/2
        top1  = y1 + fromIntegral boy1 + fromIntegral h1/2
        bottom1 = y1 + fromIntegral boy1 - fromIntegral h1/2
        left2 = x2 + fromIntegral box2 - fromIntegral w2/2
        right2 = x2 + fromIntegral box2 + fromIntegral w2/2
        bottom2 = y2 + fromIntegral boy2 - fromIntegral h2/2
checkBoundaryBoxLeftIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxLeftIntersection (V2 x1 y1) (BoundaryBox (w1, h1) (box1, boy1)) (V2 x2 y2) (BoundaryBox (w2, h2) (box2, boy2)) =
    right1 > left2 && left1 < left2 && bottom1 < top2 && top1 > bottom2
    where
        left1 = x1 + fromIntegral box1 - fromIntegral w1/2
        right1 = x1 + fromIntegral box1 + fromIntegral w1/2
        top1  = y1 + fromIntegral boy1 + fromIntegral h1/2
        bottom1 = y1 + fromIntegral boy1 - fromIntegral h1/2
        left2 = x2 + fromIntegral box2 - fromIntegral w2/2
        top2  = y2 + fromIntegral boy2 + fromIntegral h2/2
        bottom2 = y2 + fromIntegral boy2 - fromIntegral h2/2
checkBoundaryBoxRightIntersection :: V2 Float -> BoundaryBox -> V2 Float -> BoundaryBox -> Bool
checkBoundaryBoxRightIntersection (V2 x1 y1) (BoundaryBox (w1, h1) (box1, boy1)) (V2 x2 y2) (BoundaryBox (w2, h2) (box2, boy2)) =
    left1 < right2 && right1 > right2 && bottom1 < top2 && top1 > bottom2
    where
        left1 = x1 + fromIntegral box1 - fromIntegral w1/2
        right1 = x1 + fromIntegral box1 + fromIntegral w1/2
        top1  = y1 + fromIntegral boy1 + fromIntegral h1/2
        bottom1 = y1 + fromIntegral boy1 - fromIntegral h1/2
        right2 = x2 + fromIntegral box2 + fromIntegral w2/2
        top2  = y2 + fromIntegral boy2 + fromIntegral h2/2
        bottom2 = y2 + fromIntegral boy2 - fromIntegral h2/2

loadStaticSprite :: FilePath -> Picture
loadStaticSprite path = let
    res = unsafePerformIO $ loadJuicy ("assets/" ++ path)
    in case res of
        Nothing -> error $ "Failed to load sprite: " ++ path
        Just img -> img

loadAnimatedSprite :: FilePath -> Int -> (Int,Int) -> V.Vector Picture
loadAnimatedSprite path frameCount (w,h) = let
        res = unsafePerformIO $ readImage ("assets/" ++ path)
        frameWidth = w `div` frameCount
    in
        case res of
            Left err -> error $ "Failed to load sprite: " ++ path ++ " Error: " ++ err
            Right dynImg -> let
                    img = convertRGBA8 dynImg
                    subImg i = generateImage (\x y -> pixelAt img (x + (i * frameWidth)) y) frameWidth h
                in
                    V.generate frameCount (fromMaybe Blank . fromDynamicImage . ImageRGBA8 . subImg)