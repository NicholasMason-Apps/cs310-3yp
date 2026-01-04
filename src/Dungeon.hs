{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Dungeon (stepDungeon, isSpriteInView) where

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
import qualified SDL
import Enemy

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
                            GoldenReaper -> SpriteRef "golden-reaper-idle" (Just 0)
                    _ <- newEntity (CombatEnemy e, Position (V2 (1280 / 3) 0), sref)
                    set global $ CombatTurn PlayerTurn
                    startTransition (pi / 4) 1.0 ToCombat
        Nothing -> return ()

updatePlayerMovement :: System' ()
updatePlayerMovement = do
    KeysPressed ks <- get global
    mTr <- cfold (\_ (Transition {}) -> Just ()) Nothing
    if isNothing mTr then
        cmapM_ $ \(Player, Velocity _, SpriteRef sr mn, e) -> do
            let (V2 vx vy) = foldl' (\(V2 ax ay) dir -> case dir of
                    GkLeft -> V2 (ax - playerSpeed) ay
                    GkRight-> V2 (ax + playerSpeed) ay
                    GkUp -> V2 ax (ay + playerSpeed)
                    GkDown -> V2 ax (ay - playerSpeed)
                    _ -> V2 ax ay) (V2 0 0) (Set.toList ks)
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

ladderCollision :: System' ()
ladderCollision = cmapM_ $ \(Player, Position posP, bbP) -> do
    cmapM_ $ \(Ladder, Position posL, bbL) -> when (checkBoundaryBoxIntersection posP bbP posL bbL) $ do
        mTr <- cfold (\_ (Transition {}) -> Just ()) Nothing
        case mTr of
            Nothing -> startTransition (pi / 4) 1.0 ToNextLevel
            Just _ -> return ()

stepDungeon :: Float -> System' ()
stepDungeon dT = do
    updatePlayerMovement
    stepEnemyAI
    blockPlayer dT
    stepPosition dT
    handleEnemyCollisions dT
    ladderCollision

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
        if (top && vy < 0) || (bottom && vy > 0) then
            return $ Velocity (V2 avx 0)
        else if (left && vx > 0) || (right && vx < 0) then
            return $ Velocity (V2 0 avy)
        else
            return acc) (Velocity (V2 vx vy))



isSpriteInView :: Maybe Position -> Sprite -> Position -> Bool
isSpriteInView (Just (Position (V2 px py))) (Sprite (w,h) _) (Position (V2 sx sy)) =
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