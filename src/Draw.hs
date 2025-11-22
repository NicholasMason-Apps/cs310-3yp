{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Draw ( draw, getSpritePicture ) where

import Apecs
import Apecs.Gloss
import Linear
import Types
import Codec.Picture
import Graphics.Gloss.Juicy (fromDynamicImage)
import Data.Maybe
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Random
import Utils
import System.IO.Unsafe ( unsafePerformIO )
import Combat

triangle, diamond :: Picture
triangle = Line [(0,0),(-0.5,-1),(0.5,-1),(0,0)]
diamond  = Line [(-1,0),(0,-1),(1,0),(0,1),(-1,0)]

getSprite :: Map.Map String Sprite -> SpriteRef -> Sprite
getSprite smap (SpriteRef sr _) = smap Map.! sr

isSpriteInView :: Maybe (V2 Float) -> Sprite -> Position -> Bool
isSpriteInView (Just (V2 px py)) (Sprite (w,h) _) (Position (V2 sx sy)) =
    let
        vw = 1280
        vh = 720
        inViewTop = py + vh / 2 >= sy - fromIntegral h / 2
        inViewBottom = py - vh / 2 <= sy + fromIntegral h / 2
        inViewLeft = px - vw / 2 <= sx + fromIntegral w / 2
        inViewRight = px + vw / 2 >= sx - fromIntegral w / 2
    in
        inViewTop && inViewBottom && inViewLeft && inViewRight
isSpriteInView Nothing _ _ = True

drawGame :: System' Picture
drawGame = do
    KeysPressed ks <- get global
    SpriteMap smap <- get global
    playerPos <- cfold (\_ (Player, Position p) -> Just p) Nothing
    playerVelocity <- cfold (\_ (Player, Velocity v) -> Just v) Nothing
    player <- foldDraw $ \(Player, pos, s) -> let
            playerPic = getSpritePicture smap s
        in
            if KeyLeft `Set.member` ks && KeyRight `Set.notMember` ks then translate' pos $ scale (-1) 1 playerPic else translate' pos playerPic
    -- targets <- foldDraw $ \(Target, pos) -> translate' pos $ color red $ scale 10 10 diamond
    enemies <- foldDraw $ \(Enemy _, pos, s) -> translate' pos $ getSpritePicture smap s
    particles <- foldDraw $ \(Particle _, Velocity (V2 vx vy), pos) ->
        translate' pos $ color orange $ Line [(0,0),(vx/10, vy/10)]
    walls <- foldDraw $ \(Wall, pos, s) -> if isSpriteInView playerPos (getSprite smap s) pos
        then translate' pos $ getSpritePicture smap s
        else Blank
    tiles <- foldDraw $ \(Tile, pos, s) -> if isSpriteInView playerPos (getSprite smap s) pos
        then translate' pos $ getSpritePicture smap s
        else Blank
    let playerPosText = case playerPos of
            Just (V2 x y) -> color white $ translate' (Position (V2 (x-50) (y+20))) $ scale 0.1 0.1 $ Text $ "Position: (" ++ show (round x) ++ "," ++ show (round y) ++ ")"
            Nothing       -> Blank
    let playerVelocityText = case (playerVelocity, playerPos) of
            (Just (V2 vx vy), Just (V2 x y)) -> color white $ translate' (Position (V2 (x-50) (y+50))) $ scale 0.1 0.1 $ Text $ "Velocity: (" ++ show (round vx) ++ "," ++ show (round vy) ++ ")"
            _         -> Blank
    let world = tiles <> walls <> player <> enemies <> playerPosText <> playerVelocityText <> particles
    let camera = case playerPos of
            Just (V2 x y) -> translate (-x) (-y) world
            Nothing       -> world
    return camera

drawTransition :: System' Picture
drawTransition = foldDraw $ \(Transition p ang _ _) -> 
    let t = easeInOut (min 1 p)
        dist = Utils.lerp (-2000) 2000 t
        dx = dist * cos ang
        dy = dist * sin ang
        w = 2000
        h = 2000
        rect = color black $ polygon [(-w/2,-h/2), (w/2,-h/2), (w/2,h/2), (-w/2,h/2)]
    in translate dx dy $ Apecs.Gloss.rotate (ang * 180/pi) rect

draw :: System' Picture
draw = do
    gs <- get global
    Viewport (w, h) <- get global
    drawTransitionPic <- drawTransition
    let
        scaleFactorX = fromIntegral w / 1280
        scaleFactorY = fromIntegral h / 720
        scaleFactor = min scaleFactorX scaleFactorY
    p <- case gs of
        DungeonState -> drawGame
        CombatState  -> drawCombat
        _ -> return $ color white $ scale 0.3 0.3 $ Text "Menu / Paused / Game Over Screen"
    return $ scale scaleFactor scaleFactor (p <> drawTransitionPic)