{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sprite (spriteDimensions, blockPlayer, loadStaticSprite, loadAnimatedSprite, stepAnimations, stepPositionFormula, checkBoundaryBoxIntersection) where

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

-- Block the player from moving into walls
blockPlayer :: Float -> System' ()
-- blockPlayer = cmapM $ \(Wall, Position posW, spriteW) ->
--     cmapM $ \(Player, Position posP, spriteP) -> do
--         let top = checkBoundaryBoxTopIntersection posP spriteP posW spriteW
--             bottom = checkBoundaryBoxBottomIntersection posP spriteP posW spriteW
--             left = checkBoundaryBoxLeftIntersection posP spriteP posW spriteW
--             right = checkBoundaryBoxRightIntersection posP spriteP posW spriteW
--             (wp, hp) = spriteDimensions spriteP
--             (ww, hw) = spriteDimensions spriteW
--             (V2 x _) = posP
--             (V2 xw yw) = posW
--         posP' <- if top then return $ Position (V2 x (yw + (fromIntegral hw / 2) + (fromIntegral hp / 2))) else return $ Position posP
--         let (Position (V2 x' _)) = posP'
--         posP'' <- if bottom then return $ Position (V2 x' (yw - (fromIntegral hw / 2) - (fromIntegral hp / 2))) else return posP'
--         let (Position (V2 _ y'')) = posP''
--         posP''' <- if left then return $ Position (V2 (xw - (fromIntegral ww / 2) - (fromIntegral wp / 2)) y'') else return posP''
--         let (Position (V2 _ y''')) = posP'''
--         return $ if right then Position (V2 (xw + (fromIntegral ww / 2) + (fromIntegral wp / 2)) y''') else posP'''
blockPlayer t = cmapM $ \(Player, Position posP, Velocity (V2 vx vy), bbp) -> do
    let (Position tempPos) = stepPositionFormula t (Position posP) (Velocity (V2 vx vy))
    cfoldM (\acc (Wall, Position posW, bbw) -> do
        let
            top = checkBoundaryBoxTopIntersection tempPos bbp posW bbw
            bottom = checkBoundaryBoxBottomIntersection tempPos bbp posW bbw
            left = checkBoundaryBoxLeftIntersection tempPos bbp posW bbw
            right = checkBoundaryBoxRightIntersection tempPos bbp posW bbw
            (Velocity (V2 avx avy)) = acc
        if (top && vy < 0) || (bottom && vy > 0) then
            return $ Velocity (V2 avx 0)
        else if (left && vx > 0) || (right && vx < 0) then
            return $ Velocity (V2 0 avy)
        else
            return acc) (Velocity (V2 vx vy))

stepAnimations :: Float -> System' ()
stepAnimations dT = do
    cmapM $ \(SpriteRef sr e) -> do
        Time t <- get global
        SpriteMap smap <- get global
        let Sprite _ spriteE = smap ! sr
        case spriteE of
            Left _ -> return $ SpriteRef sr e
            Right a -> let
                    trigger = floor (t / frameSpeed a) /= floor ((t + dT) / frameSpeed a)
                in return $ updateAnimation (SpriteRef sr e) trigger (frameCount a)
    where
        updateAnimation :: SpriteRef -> Bool -> Int -> SpriteRef
        updateAnimation (SpriteRef sr Nothing) _ _ = SpriteRef sr Nothing
        updateAnimation (SpriteRef sr (Just a)) trigger fc =
            if trigger
            then let
                newFrame = (a + 1) `mod` fc
                in SpriteRef sr (Just newFrame)
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