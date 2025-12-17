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
    spriteList <- liftIO $ sequence [ do
        s <- getTileSprite
        let sref = SpriteRef s Nothing
            pos = Position (V2 (offsetX + fromIntegral x * tileSize) (offsetY + fromIntegral y * tileSize))
        return (sref, pos)
        | x <- [0..ceiling (1280 / tileSize)], y <- [0..ceiling (720 / tileSize)] ]
    forM_ spriteList $ \(s, p) -> do
        void $ newEntity (CombatTile, p, s)

incrementTime :: Float -> System' ()
incrementTime dT = modify global $ \(Time t) -> Time (t + dT)

-- Remove all particles that have expired
stepParticles :: Float -> System' ()
stepParticles dT = cmap $ \(Particle t) ->
    if t < 0
        then Right $ Not @(Particle, Kinetic) -- Not is used to delete the particle
        else Left $ Particle (t - dT)

triggerEvery :: Float -> Float -> Float -> System' a -> System' ()
triggerEvery dT period offset sys = do
    Time t <- get global
    let t' = t + offset
        trigger = floor (t'/period) /= floor ((t'+dT)/period)
    when trigger $ void sys

-- Spawn n particles at a given position with random velocities in the given ranges
-- dvx and dvy are (min,max) pairs for x and y velocity components
spawnParticles :: Int -> Position -> (Float,Float) -> (Float,Float) -> System' ()
spawnParticles n pos dvx dvy = replicateM_ n $ do
    vx <- liftIO $ randomRIO dvx
    vy <- liftIO $ randomRIO dvy
    t <- liftIO $ randomRIO (0.02, 0.3)
    newEntity (Particle t, pos, Velocity (V2 vx vy))

combatTransitionAction :: System' ()
combatTransitionAction = do
    liftIO $ putStrLn "Enemy defeated! Returning to dungeon..."
    ce <- cfold (\_ (CombatEnemy ce) -> Just ce) Nothing
    case ce of
        Nothing -> return ()
        Just e -> do
            destroy e (Proxy @(Enemy, SpriteRef, Position, Velocity, Health))
            cmapM_ $ \(CombatEnemy _, e') -> destroy e' (Proxy @(CombatEnemy, SpriteRef, Position))
            set global DungeonState

dungeonTransitionAction :: System' ()
dungeonTransitionAction = do
    set global CombatState

stepTransition :: Float -> System' ()
stepTransition dT = cmapM_ $ \(Transition p ang spd fired, e) -> do
    let p' = p + dT * spd
    when (not fired && p' >= 0.5) $ do
        state <- get global :: System' GameState
        case state of
            DungeonState -> dungeonTransitionAction
            CombatState -> combatTransitionAction
            _ -> return ()
    if p' >= 1 then
        destroy e (Proxy @Transition)
    else
        set e Transition { trProgress = p', trAngle = ang, trSpeed = spd, trCoverEventFired = (fired || p' >= 0.5) }

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