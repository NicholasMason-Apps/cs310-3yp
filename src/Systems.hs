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
initialize :: System' ()
initialize = do
    let spriteList = [
                        (
                            "player-idle",
                            Sprite (64,64) (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = loadAnimatedSprite "player/idle.png" 6 (384,64), looping = (True, Nothing) })
                        ),
                        (
                            "player-walk",
                            Sprite (64,64) (Right $ Animation { frameCount = 10, frameSpeed = 0.1, sprites = loadAnimatedSprite "player/walk.png" 10 (640,64), looping = (True, Nothing) })
                        ),
                        (
                            "player-knife-attack",
                            Sprite (64,64) (Right $ Animation { frameCount = 9, frameSpeed = 0.1, sprites = loadAnimatedSprite "player/knife-attack.png" 9 (576,64), looping = (False, Just "player-idle") })
                        ),
                        (
                            "player-staff",
                            Sprite (64,64) (Right $ Animation { frameCount = 7, frameSpeed = 0.3, sprites = loadAnimatedSprite "player/player-staff.png" 7 (448,64), looping = (False, Just "player-idle") })
                        ),
                        (
                            "player-hit",
                            Sprite (64,64) (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = loadAnimatedSprite "player/hit.png" 5 (320,64), looping = (False, Just "player-idle") })
                        ),
                        (
                            "skeleton-idle",
                            Sprite (64,64) (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = loadAnimatedSprite "enemies/skeleton/idle.png" 6 (384,64), looping = (True, Nothing) })
                        ),
                        (
                            "skeleton-walk",
                            Sprite (64,64) (Right $ Animation { frameCount = 10, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/skeleton/walk.png" 10 (640,64), looping = (True, Nothing) })
                        ),
                        (
                            "skeleton-attack",
                            Sprite (64,64) (Right $ Animation { frameCount = 9, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/skeleton/attack.png" 9 (576,64), looping = (False, Just "skeleton-idle") })
                        ),
                        (
                            "skeleton-hit",
                            Sprite (64,64) (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/skeleton/hit.png" 5 (320,64), looping = (False, Just "skeleton-idle") })
                        ),
                        (
                            "reaper-idle",
                            Sprite (64,64) (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = loadAnimatedSprite "enemies/reaper/idle.png" 6 (384,64), looping = (True, Nothing) })
                        ),
                        (
                            "reaper-walk",
                            Sprite (64,64) (Right $ Animation { frameCount = 8, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/reaper/walk.png" 8 (512,64), looping = (True, Nothing) })
                        ),
                        (
                            "reaper-attack",
                            Sprite (64,64) (Right $ Animation { frameCount = 15, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/reaper/attack.png" 15 (960,64), looping = (False, Just "reaper-idle") })
                        ),
                        (
                            "reaper-hit",
                            Sprite (64,64) (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/reaper/hit.png" 5 (320,64), looping = (False, Just "reaper-idle") })
                        ),
                        (
                            "vampire-idle",
                            Sprite (64,64) (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = loadAnimatedSprite "enemies/vampire/idle.png" 6 (384,64), looping = (True, Nothing) })
                        ),
                        (
                            "vampire-walk",
                            Sprite (64,64) (Right $ Animation { frameCount = 8, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/vampire/walk.png" 8 (512,64), looping = (True, Nothing) })
                        ),
                        (
                            "vampire-attack",
                            Sprite (64,64) (Right $ Animation { frameCount = 16, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/vampire/attack.png" 16 (1024,64), looping = (False, Just "vampire-idle") })
                        ),
                        (
                            "vampire-hit",
                            Sprite (64,64) (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/vampire/hit.png" 5 (320,64), looping = (False, Just "vampire-idle") })
                        )
                     ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..tileCount], let name = "tile" ++ show n, let path = "tiles/tile" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallTopCount], let name = "wall-top" ++ show n, let path = "tiles/wall-top" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallBottomCount], let name = "wall-bottom" ++ show n, let path = "tiles/wall-bottom" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallLeftCount], let name = "wall-left" ++ show n, let path = "tiles/wall-left" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallRightCount], let name = "wall-right" ++ show n, let path = "tiles/wall-right" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallBottomLeftElbowCount], let name = "wall-bottom-left-elbow" ++ show n, let path = "tiles/wall-bottom-left-elbow" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallBottomRightElbowCount], let name = "wall-bottom-right-elbow" ++ show n, let path = "tiles/wall-bottom-right-elbow" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ 
                        ("wall-bottom-right", Sprite (64,64) (Left $ loadStaticSprite "tiles/wall-bottom-right.png") ), 
                        ("wall-bottom-left", Sprite (64,64) (Left $ loadStaticSprite "tiles/wall-bottom-left.png") ),
                        ("combat-ui", Sprite (1280,720) (Left $ loadStaticSprite "ui/combat-ui.png") )
                    ]
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
stepTransition dT = cmapM $ \(Transition p ang spd fired, e) -> do
    let p' = p + dT * spd
    when (not fired && p' >= 0.5) $ do
        state <- get global :: System' GameState
        case state of
            DungeonState -> dungeonTransitionAction
            CombatState -> combatTransitionAction
            _ -> return ()
    when (p' >= 1) $ destroy e (Proxy @Transition)
    return $ Transition { trProgress = p', trAngle = ang, trSpeed = spd, trCoverEventFired = (fired || p' >= 0.5) }

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

handleEvent :: Event -> System' ()
-- Player movement
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent (EventKey (SpecialKey k) Down _ _) = modify global $ \(KeysPressed ks) -> KeysPressed (Set.insert (SpecialKey k) ks)
handleEvent (EventKey (SpecialKey k) Up _ _) = modify global $ \(KeysPressed ks) -> KeysPressed (Set.delete (SpecialKey k) ks)
-- Exit game
handleEvent (EventKey (Char 'e') Down _ _) = modify global $ \(KeysPressed ks) -> KeysPressed (Set.insert (Char 'e') ks)
handleEvent (EventKey (Char 'e') Up _ _) = modify global $ \(KeysPressed ks) -> KeysPressed (Set.delete (Char 'e') ks)
handleEvent (EventResize sz) = set global (Viewport sz)
handleEvent _ = return () -- base case