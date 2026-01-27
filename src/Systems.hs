{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Systems where

import Apecs
import Apecs.Gloss
import System.Random
import System.Exit
import Linear
import Control.Monad
import Types
import Sprite
import GameMap
import Data.Set (Set)
import qualified Data.Set as Set
import Utils
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (foldl')
import Data.Maybe
import System.IO.Unsafe ( unsafePerformIO )
import Combat
import Dungeon

-- Initialise the game state by creating a player entity
initialize :: [(String, Sprite)] -> System' ()
initialize spriteList = do
    set global (SpriteMap $ Map.fromList spriteList)
    playerEntity <- newEntity (Player, Position playerPos, Velocity (V2 0 0), SpriteRef "player-idle" (Just 0),  BoundaryBox (16, 26) (0, -11), Health 100)
    combatPlayerEntity <- newEntity (CombatPlayer, Position (V2 (-1280 / 3) 0), Velocity (V2 0 0), SpriteRef "player-idle" (Just 0))
    generateMap
    let offsetX = tileSize / 2 - 1280/2
        offsetY = tileSize / 2 - 720/2
        getTileSprite :: IO String
        getTileSprite = do
            n <- randomRIO (1,tileCount) :: IO Integer
            return $ "tile" ++ show n
    tileList <- liftIO $ sequence [ do
        s <- getTileSprite
        let sref = SpriteRef s Nothing
            pos = Position (V2 (offsetX + fromIntegral x * tileSize) (offsetY + fromIntegral y * tileSize))
        return (sref, pos)
        | x <- [0..ceiling (1280 / tileSize)], y <- [0..ceiling (720 / tileSize)] ]
    forM_ tileList $ \(s, p) -> do
        void $ newEntity (CombatTile, p, s)

incrementTime :: Float -> System' ()
incrementTime dT = modify global $ \(Time t) -> Time (t + dT)

-- Remove Velocity component from particles whose destination position has been reached
-- When a particle finishes its animation, remove it from the world
stepParticles :: Float -> System' ()
stepParticles dT = cmapM_ $ \(Particle (Position destP), Position currP, SpriteRef sref mn, e) -> do
    SpriteMap smap <- get global
    when (norm (destP - currP) < 5) $ do
        destroy e (Proxy @Velocity)
    let Sprite _ rs = smap Map.! sref
    case rs of
        GlossRenderer (Right a) -> when (fromMaybe 0 mn + 1 >= frameCount a) $ do
            destroy e (Proxy @(Particle, SpriteRef, Position))
            cmapM_ $ \(CombatAttackParticle _, e') -> destroy e' (Proxy @CombatAttackParticle)
        SDLRenderer (_, Just a) -> when (fromMaybe 0 mn + 1 >= frameCount a) $ do
            destroy e (Proxy @(Particle, SpriteRef, Position))
            cmapM_ $ \(CombatAttackParticle _, e') -> destroy e' (Proxy @CombatAttackParticle)
        RaylibRenderer (_, Just a) -> when (fromMaybe 0 mn + 1 >= frameCount a) $ do
            destroy e (Proxy @(Particle, SpriteRef, Position))
            cmapM_ $ \(CombatAttackParticle _, e') -> destroy e' (Proxy @CombatAttackParticle)
        _ -> return ()


triggerEvery :: Float -> Float -> Float -> System' a -> System' ()
triggerEvery dT period offset sys = do
    Time t <- get global
    let t' = t + offset
        trigger = floor (t'/period) /= floor ((t'+dT)/period)
    when trigger $ void sys

toDungeonAction :: System' ()
toDungeonAction = do
    ce <- cfold (\_ (CombatEnemy ce) -> Just ce) Nothing
    case ce of
        Nothing -> return ()
        Just e -> do
            destroy e (Proxy @(Enemy, SpriteRef, Position, Velocity, Health))
            cmapM_ $ \(CombatEnemy _, e') -> destroy e' (Proxy @(CombatEnemy, SpriteRef, Position))
            set global DungeonState

toCombatAction :: System' ()
toCombatAction = do
    set global CombatState

toNextLevelAction :: System' ()
toNextLevelAction = do
    -- Destroy all Walls, Floors, etc.
    cmapM_ $ \(Wall, e) -> destroy e (Proxy @(Wall, Tile, Position, SpriteRef, BoundaryBox))
    cmapM_ $ \(Ladder, e) -> destroy e (Proxy @(Ladder, Tile, Position, SpriteRef, BoundaryBox))
    cmapM_ $ \(Tile, e) -> destroy e (Proxy @(Tile, Position, SpriteRef))
    cmapM_ $ \(Enemy _, e) -> destroy e (Proxy @(Enemy, Position, Velocity, Health, SpriteRef, BoundaryBox))
    cmap $ \(Player, Position _) -> Position playerPos 
    generateMap

stepTransition :: Float -> System' ()
stepTransition dT = cmapM_ $ \(Transition p ang spd fired event, e) -> do
    let p' = p + dT * spd
    when (not fired && p' >= 0.5) $ case event of
        ToCombat -> toCombatAction
        ToDungeon -> toDungeonAction
        ToNextLevel -> toNextLevelAction
    if p' >= 1 then
        destroy e (Proxy @Transition)
    else
        set e Transition { trProgress = p', trAngle = ang, trSpeed = spd, trCoverEventFired = fired || p' >= 0.5, trEvent = event }

-- TODO: Add boundary box collision check and stop player movement
step :: Float -> System' ()
step dT = do
    gs <- get global
    incrementTime dT
    stepAnimations dT
    stepParticles dT
    stepTransition dT
    case gs of
        DungeonState -> stepDungeon dT
        CombatState  -> stepCombat dT
        _            -> return ()