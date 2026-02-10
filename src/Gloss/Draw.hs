{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gloss.Draw ( draw, getSpritePicture ) where

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
import Systems
import Dungeon

getSprite :: Map.Map String Sprite -> SpriteRef -> Sprite
getSprite smap (SpriteRef sr _) = smap Map.! sr

drawTransition :: System' Picture
drawTransition = foldDraw $ \(Transition p ang _ _ _) -> 
    let t = easeInOut (min 1 p)
        dist = Utils.lerp (-2000) 2000 t
        dx = dist * cos ang
        dy = dist * sin ang
        w = 2500
        h = 2500
        rect = color black $ polygon [(-w/2,-h/2), (w/2,-h/2), (w/2,h/2), (-w/2,h/2)]
    in translate dx dy $ Apecs.Gloss.rotate (ang * 180/pi) rect

draw :: System' Picture
draw = do
    gs <- get global
    SpriteMap smap <- get global
    Viewport (w, h) <- get global
    drawTransitionPic <- drawTransition
    particles <- foldDraw $ \(Particle _, pos, s) -> translate' pos $ getSpritePicture smap s
    let
        scaleFactorX = fromIntegral w / 1280
        scaleFactorY = fromIntegral h / 720
        scaleFactor = min scaleFactorX scaleFactorY
    p <- case gs of
        DungeonState -> drawDungeon
        CombatState  -> drawCombat
        _ -> return $ color white $ scale 0.3 0.3 $ Text "Menu / Paused / Game Over Screen"
    return $ scale scaleFactor scaleFactor (p <> particles <> drawTransitionPic ) -- <> playerHealth)

drawDungeon :: System' Picture
drawDungeon = do
    KeysPressed ks <- get global
    SpriteMap smap <- get global
    playerPos <- cfold (\_ (Player, p) -> Just p) Nothing
    playerVelocity <- cfold (\_ (Player, Velocity v) -> Just v) Nothing
    player <- foldDraw $ \(Player, pos, s) -> let
            playerPic = getSpritePicture smap s
        in
            if GkLeft `Set.member` ks && GkRight `Set.notMember` ks then translate' pos $ scale (-1) 1 playerPic else translate' pos playerPic
    playerBox <- foldDraw $ \(Player, BoundaryBox (w,h) (ox,oy)) -> let
            boxPic = color green $ rectangleWire (fromIntegral w) (fromIntegral h)
        in
            case playerPos of
                Just (Position (V2 px py)) -> translate' (Position (V2 (px + fromIntegral ox) (py + fromIntegral oy))) boxPic
                Nothing         -> Blank
    -- targets <- foldDraw $ \(Target, pos) -> translate' pos $ color red $ scale 10 10 diamond
    enemies <- foldDraw $ \(Enemy _, pos, s) -> translate' pos $ getSpritePicture smap s
    enemyBoxes <- foldDraw $ \(Enemy _, Position (V2 x y), BoundaryBox (w,h) (ox,oy)) -> let
            boxPic = color blue $ rectangleWire (fromIntegral w) (fromIntegral h)
        in
            translate' (Position (V2 (x + fromIntegral ox) (y + fromIntegral oy))) boxPic
    tiles <- foldDraw $ \(Tile, pos, s) -> if isSpriteInView playerPos (getSprite smap s) pos
        then translate' pos $ getSpritePicture smap s
        else Blank
    let playerPosText = case playerPos of
            Just (Position (V2 x y)) -> color white $ translate' (Position (V2 (x-50) (y+20))) $ scale 0.1 0.1 $ Text $ "Position: (" ++ show (round x) ++ "," ++ show (round y) ++ ")"
            Nothing       -> Blank
    let playerVelocityText = case (playerVelocity, playerPos) of
            (Just (V2 vx vy), Just (Position (V2 x y))) -> color white $ translate' (Position (V2 (x-50) (y+50))) $ scale 0.1 0.1 $ Text $ "Velocity: (" ++ show (round vx) ++ "," ++ show (round vy) ++ ")"
            _         -> Blank
    playerHealth <- foldDraw $ \(Player, Health hp) -> color white . translate (-600) 300 . scale 0.3 0.3 . Text $ "Health: " ++ show hp
    let world = tiles <> player <> enemies <>  playerHealth
    let camera = case playerPos of
            Just (Position (V2 x y)) -> translate (-x) (-y) world
            Nothing       -> world
    return camera

drawCombat :: System' Picture
drawCombat = do
    SpriteMap smap <- get global
    CombatTurn turn <- get global
    uiState <- get global :: System' UIState
    let ui = if turn == PlayerTurn then case uiState of
            CombatAttackSelectUI -> getSpritePicture smap (SpriteRef "combat-attack-select-ui" Nothing)
            CombatMagicSelectUI  -> getSpritePicture smap (SpriteRef "combat-magic-select-ui" Nothing)
        else Blank
    player <- foldDraw $ \(CombatPlayer, pos, s) -> translate' pos $ scale 2 2 $ getSpritePicture smap s
    enemy <- foldDraw $ \(CombatEnemy _, pos, s) -> translate' pos $ scale (-2) 2 $ getSpritePicture smap s
    enemyPlayerLayer <- if turn == PlayerTurn || turn == PlayerAttacking then
            return $ enemy <> player
        else
            return $ player <> enemy
    tiles <- foldDraw $ \(CombatTile, pos, s) -> translate' pos $ getSpritePicture smap s
    playerHealth <- foldDraw $ \(Player, Health hp) -> color white . translate (-600) 300 . scale 0.3 0.3 . Text $ "Health: " ++ show hp
    return $ tiles <> enemyPlayerLayer <> ui <> playerHealth