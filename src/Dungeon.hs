{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dungeon (stepDungeon, drawDungeon) where

import Apecs
import Apecs.Gloss
import Linear
import Types
import Sprite
import qualified Data.Set as Set
import Utils
import Data.Foldable (foldl')
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad
import Enemy
import qualified SDL

handleEnemyCollisions :: Float -> System' ()
handleEnemyCollisions dT = cmapM_ $ \(Player, Position posP, v, bbp) -> do
    enemyRes <- cfold (\acc (Enemy _, Position posE, bbE, e) ->
        let
            (Position tempPosP) = stepPositionFormula dT (Position posP) v
        in
        if checkBoundaryBoxIntersection tempPosP bbp posE bbE && isNothing acc then
            Just e
        else
            acc) Nothing
    case enemyRes of
        Just e -> do
            ce <- cfold (\_ (CombatEnemy ce) -> Just ce) Nothing
            case ce of
                Just _ -> return ()
                Nothing -> do
                    et <- get e :: System' Enemy
                    let sref = case enemyType et of
                            Reaper -> SpriteRef "reaper-idle" (Just 0)
                            Vampire -> SpriteRef "vampire-idle" (Just 0)
                            Skeleton -> SpriteRef "skeleton-idle" (Just 0)
                    _ <- newEntity (CombatEnemy e, Position (V2 (1280 / 3) 0), sref)
                    set global $ CombatTurn PlayerTurn
                    startTransition (pi / 4) 1.0
        Nothing -> return ()

updatePlayerMovement :: System' ()
updatePlayerMovement = do
    KeysPressed ks <- get global
    enemy <- cfold (\_ (CombatEnemy ce) -> Just ce) Nothing
    if isNothing enemy then
        cmapM_ $ \(Player, Velocity _, SpriteRef sr mn, e) -> do
            let (V2 vx vy) = foldl' (\(V2 ax ay) dir -> case dir of
                                            SDL.KeycodeLeft  -> V2 (ax - playerSpeed) ay
                                            SDL.KeycodeRight -> V2 (ax + playerSpeed) ay
                                            SDL.KeycodeUp    -> V2 ax (ay - playerSpeed)
                                            SDL.KeycodeDown  -> V2 ax (ay + playerSpeed)
                                            _        -> V2 ax ay) (V2 0 0) (Set.toList ks)
                newSprite
                    | vx == 0 && vy == 0 && sr /= "player-idle" = SpriteRef "player-idle" (Just 0)
                    | (vx /= 0 || vy /= 0) && sr /= "player-walk" = SpriteRef "player-walk" (Just 0)
                    | otherwise = SpriteRef sr mn
            set e (Velocity (V2 vx vy))
            set e newSprite
    else
        cmapM_ $ \(Player, e) -> do
            set e (Velocity (V2 0 0))
            set e (SpriteRef "player-idle" (Just 0))

stepDungeon :: Float -> System' ()
stepDungeon dT = do
    updatePlayerMovement
    stepEnemyAI
    blockPlayer dT
    stepPosition dT
    handleEnemyCollisions dT

-- Block the player from moving into walls
blockPlayer :: Float -> System' ()
blockPlayer t = cmapM $ \(Player, Position posP, Velocity (V2 vx vy), bbp) -> do
    let (Position tempPos) = stepPositionFormula t (Position posP) (Velocity (V2 vx vy))
    cfoldM (\acc (Wall, Position posW, bbw) -> do
        let
            top = checkBoundaryBoxTopIntersection tempPos bbp posW bbw
            bottom = checkBoundaryBoxBottomIntersection tempPos bbp posW bbw
            left = checkBoundaryBoxLeftIntersection tempPos bbp posW bbw
            right = checkBoundaryBoxRightIntersection tempPos bbp posW bbw
            (Velocity (V2 avx avy)) = acc
        if (top && vy > 0) || (bottom && vy < 0) then
            return $ Velocity (V2 avx 0)
        else if (left && vx > 0) || (right && vx < 0) then
            return $ Velocity (V2 0 avy)
        else
            return acc) (Velocity (V2 vx vy))

getSprite :: SpriteMap -> SpriteRef -> Sprite
getSprite (SpriteMap smap) (SpriteRef sr _) = smap Map.! sr

isSpriteInView :: Maybe (V2 Float) -> Sprite -> Position -> Bool
isSpriteInView (Just (V2 px py)) (Sprite (w,h) _ _) (Position (V2 sx sy)) =
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

drawDungeon :: SDL.Renderer -> FPS -> System' ()
drawDungeon r fps = do
    smap <- get global :: System' SpriteMap
    playerPos <- cfold (\_ (Player, Position p) -> Just p) Nothing
    let (psx, psy) = (64,64)
        worldToScreen (V2 x y) = case playerPos of
            Just (V2 px py) -> V2 (x - px + 1280 / 2 - fromIntegral psx / 2) (y - py + 720 / 2 - fromIntegral psy / 2)
            Nothing         -> V2 x y
    cmapM_ $ \(Player, Position posP, sref) -> liftIO $ drawSprite sref smap (Position $ worldToScreen posP) r
    cmapM_ $ \(Wall, Position posW, sref) -> when (isSpriteInView playerPos (getSprite smap sref) (Position posW)) $ liftIO $ drawSprite sref smap (Position $ worldToScreen posW) r
    -- playerPos <- cfold (\_ (Player, Position p) -> Just p) Nothing
    -- playerVelocity <- cfold (\_ (Player, Velocity v) -> Just v) Nothing
    -- player <- foldDraw $ \(Player, pos, s) -> let
    --         playerPic = getSpritePicture smap s
    --     in
    --         if SDL.KeycodeLeft `Set.member` ks && SDL.KeycodeRight `Set.notMember` ks then translate' pos $ scale (-1) 1 playerPic else translate' pos playerPic
    -- playerBox <- foldDraw $ \(Player, BoundaryBox (w,h) (ox,oy)) -> let
    --         boxPic = color green $ rectangleWire (fromIntegral w) (fromIntegral h)
    --     in
    --         case playerPos of
    --             Just (V2 px py) -> translate' (Position (V2 (px + fromIntegral ox) (py + fromIntegral oy))) boxPic
    --             Nothing         -> Blank
    -- -- targets <- foldDraw $ \(Target, pos) -> translate' pos $ color red $ scale 10 10 diamond
    -- enemies <- foldDraw $ \(Enemy _, pos, s) -> translate' pos $ getSpritePicture smap s
    -- enemyBoxes <- foldDraw $ \(Enemy _, Position (V2 x y), BoundaryBox (w,h) (ox,oy)) -> let
    --         boxPic = color blue $ rectangleWire (fromIntegral w) (fromIntegral h)
    --     in
    --         translate' (Position (V2 (x + fromIntegral ox) (y + fromIntegral oy))) boxPic
    -- particles <- foldDraw $ \(Particle _, Velocity (V2 vx vy), pos) ->
    --     translate' pos $ color orange $ Line [(0,0),(vx/10, vy/10)]
    -- walls <- foldDraw $ \(Wall, pos, s) -> if isSpriteInView playerPos (getSprite smap s) pos
    --     then translate' pos $ getSpritePicture smap s
    --     else Blank
    -- tiles <- foldDraw $ \(Tile, pos, s) -> if isSpriteInView playerPos (getSprite smap s) pos
    --     then translate' pos $ getSpritePicture smap s
    --     else Blank
    -- let playerPosText = case playerPos of
    --         Just (V2 x y) -> color white $ translate' (Position (V2 (x-50) (y+20))) $ scale 0.1 0.1 $ Text $ "Position: (" ++ show (round x) ++ "," ++ show (round y) ++ ")"
    --         Nothing       -> Blank
    -- let playerVelocityText = case (playerVelocity, playerPos) of
    --         (Just (V2 vx vy), Just (V2 x y)) -> color white $ translate' (Position (V2 (x-50) (y+50))) $ scale 0.1 0.1 $ Text $ "Velocity: (" ++ show (round vx) ++ "," ++ show (round vy) ++ ")"
    --         _         -> Blank
    -- let world = tiles <> walls <> player <> enemies <> playerPosText <> playerVelocityText <> playerBox <> enemyBoxes <> particles
    -- let camera = case playerPos of
    --         Just (V2 x y) -> translate (-x) (-y) world
    --         Nothing       -> world
    -- return camera