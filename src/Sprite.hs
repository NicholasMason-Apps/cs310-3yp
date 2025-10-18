{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sprite (spriteDimensions, blockPlayer, loadSpritePicture) where

import Apecs
import Apecs.Gloss
import System.Random
import System.Exit
import Linear
import Control.Monad
import Types
import Data.Maybe ( fromMaybe )
import System.IO.Unsafe ( unsafePerformIO )
-- import Graphics.Gloss.Juicy ( loadJuicy )

spriteDimensions :: Sprite -> (Int, Int)
spriteDimensions (Sprite s (w,h) Nothing) = (w, h)
spriteDimensions (Sprite _ (w,h) (Just a)) = (w `div` frameCount a, h)

-- Block the player from moving into walls
blockPlayer :: System' ()
blockPlayer = cmapM $ \(Wall, Position posW, spriteW) ->
    cmapM $ \(Player, Position posP, spriteP) -> do
        let top = checkBoundaryBoxTopIntersection posP spriteP posW spriteW
            bottom = checkBoundaryBoxBottomIntersection posP spriteP posW spriteW
            left = checkBoundaryBoxLeftIntersection posP spriteP posW spriteW
            right = checkBoundaryBoxRightIntersection posP spriteP posW spriteW
            (wp, hp) = spriteDimensions spriteP
            (ww, hw) = spriteDimensions spriteW
            (V2 x _) = posP
            (V2 xw yw) = posW
        posP' <- if top then return $ Position (V2 x (yw + (fromIntegral hw / 2) + (fromIntegral hp / 2))) else return $ Position posP
        let (Position (V2 x' _)) = posP'
        posP'' <- if bottom then return $ Position (V2 x' (yw - (fromIntegral hw / 2) - (fromIntegral hp / 2))) else return posP'
        let (Position (V2 _ y'')) = posP''
        posP''' <- if left then return $ Position (V2 (xw - (fromIntegral ww / 2) - (fromIntegral wp / 2)) y'') else return posP''
        let (Position (V2 _ y''')) = posP'''
        return $ if right then Position (V2 (xw + (fromIntegral ww / 2) + (fromIntegral wp / 2)) y''') else posP'''

animateSprites :: System' ()
animateSprites = cmapM $ \(Sprite pic (w,h) ma) -> do
    Time t <- get global
    FPS fps <- get global -- look into bitmaps
    case ma of
        Nothing -> return $ Sprite pic (w,h) Nothing
        Just a -> do
            let timeSinceLast = timeSinceLastFrame a + (1 / fromIntegral fps)
                frameDur = 1 / frameSpeed a
                (newFrame, newTimeSinceLast) = if timeSinceLast >= frameDur
                                              then ((currentFrame a `mod` frameCount a) + 1, timeSinceLast - frameDur)
                                              else (currentFrame a, timeSinceLast)
            return $ Sprite pic (w,h) (Just a { currentFrame = newFrame, timeSinceLastFrame = newTimeSinceLast })
    

--     TODO: add animation rendering by figuring out how to do a rendering rectangle
--     also need to add a frame time component to control animation speed

-- Boundary box collision detection
-- Note: Sprite positions are centered based on their Position component
checkBoundaryBoxTopIntersection :: V2 Float -> Sprite -> V2 Float -> Sprite -> Bool
checkBoundaryBoxTopIntersection (V2 x1 y1) s1 (V2 x2 y2) s2 =
    top1 < bottom2 && bottom1 > bottom2 && right1 > left2 && left1 < right2
    where
        (w1,h1) = spriteDimensions s1
        (w2,h2) = spriteDimensions s2
        left1 = x1 - fromIntegral w1/2
        right1 = x1 + fromIntegral w1/2
        top1  = y1 - fromIntegral h1/2
        bottom1 = y1 + fromIntegral h1/2
        left2 = x2 - fromIntegral w2/2
        right2 = x2 + fromIntegral w2/2
        bottom2 = y2 + fromIntegral h2/2
checkBoundaryBoxBottomIntersection :: V2 Float -> Sprite -> V2 Float -> Sprite -> Bool
checkBoundaryBoxBottomIntersection (V2 x1 y1) s1 (V2 x2 y2) s2 =
    bottom1 > top2 && top1 < top2 && right1 > left2 && left1 < right2
    where
        (w1,h1) = spriteDimensions s1
        (w2,h2) = spriteDimensions s2
        left1 = x1 - fromIntegral w1/2
        right1 = x1 + fromIntegral w1/2
        top1  = y1 - fromIntegral h1/2
        bottom1 = y1 + fromIntegral h1/2
        left2 = x2 - fromIntegral w2/2
        right2 = x2 + fromIntegral w2/2
        top2  = y2 - fromIntegral h2/2
checkBoundaryBoxLeftIntersection :: V2 Float -> Sprite -> V2 Float -> Sprite -> Bool
checkBoundaryBoxLeftIntersection (V2 x1 y1) s1 (V2 x2 y2) s2 =
    right1 > left2 && left1 < left2 && bottom1 > top2 && top1 < bottom2
    where
        (w1,h1) = spriteDimensions s1
        (w2,h2) = spriteDimensions s2
        left1 = x1 - fromIntegral w1/2
        right1 = x1 + fromIntegral w1/2
        top1  = y1 - fromIntegral h1/2
        bottom1 = y1 + fromIntegral h1/2
        left2 = x2 - fromIntegral w2/2
        top2  = y2 - fromIntegral h2/2
        bottom2 = y2 + fromIntegral h2/2
checkBoundaryBoxRightIntersection :: V2 Float -> Sprite -> V2 Float -> Sprite -> Bool
checkBoundaryBoxRightIntersection (V2 x1 y1) s1 (V2 x2 y2) s2 =
    left1 < right2 && right1 > right2 && bottom1 > top2 && top1 < bottom2
    where
        (w1,h1) = spriteDimensions s1
        (w2,h2) = spriteDimensions s2
        left1 = x1 - fromIntegral w1/2
        right1 = x1 + fromIntegral w1/2
        top1  = y1 - fromIntegral h1/2
        bottom1 = y1 + fromIntegral h1/2
        right2 = x2 + fromIntegral w2/2
        top2  = y2 - fromIntegral h2/2
        bottom2 = y2 + fromIntegral h2/2

loadSpritePicture :: FilePath -> Picture 
loadSpritePicture path = fromMaybe (text "Error loading sprite") $ unsafePerformIO $ loadJuicy ("assets/" ++ path)