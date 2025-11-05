{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sprite (spriteDimensions, blockPlayer, loadStaticSprite, loadAnimatedSprite, stepAnimations) where

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

spriteDimensions :: Sprite -> (Int, Int)
spriteDimensions (Sprite (w,h) (Left _)) = (w, h)
spriteDimensions (Sprite (w,h) (Right a)) = (w `div` frameCount a, h)

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

stepAnimations :: Float -> System' ()
stepAnimations dT = do
    stepPlayerAnimations dT
    stepNonPlayerAnimations dT
    where
        updateAnimation :: Sprite -> Bool -> Sprite
        updateAnimation (Sprite (w,h) (Left l)) _ = Sprite (w,h) (Left l)
        updateAnimation (Sprite (w,h) (Right a)) trigger =
            if trigger
            then let
                newFrame = (currentFrame a `mod` frameCount a) + 1
                in Sprite (w,h) (Right a { currentFrame = newFrame })
            else Sprite (w,h) (Right a)
        stepPlayerAnimations :: Float -> System' ()
        stepPlayerAnimations dT = cmapM $ \(Player, Sprite (w,h) e, MoveDirection md) -> do
            Time t <- get global
            case e of
                Left l -> return $ Sprite (w,h) (Left l)
                Right a -> let
                        trigger = floor (t / frameSpeed a) /= floor ((t + dT) / frameSpeed a)
                    in return $ if isJust md
                        then updateAnimation (Sprite (w,h) (Right a)) trigger
                        else Sprite (w,h) (Right a { currentFrame = 1 })
        stepNonPlayerAnimations :: Float -> System' ()
        stepNonPlayerAnimations dT = cmapM_ $ \(Sprite (w,h) e', e) -> do
            Time t <- get global
            isPlayer <- exists e (Proxy @Player)
            unless isPlayer $ case e' of
                Left _ -> return ()
                Right a -> let
                        trigger = floor (t / frameSpeed a) /= floor ((t + dT) / frameSpeed a)
                    in set e $ updateAnimation (Sprite (w,h) (Right a)) trigger

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