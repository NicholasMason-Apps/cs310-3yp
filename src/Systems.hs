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

playerSpeed, bulletSpeed, enemySpeed, xmin, xmax :: Float
playerSpeed = 170
bulletSpeed = 500
enemySpeed  = 80
xmin = -640
xmax = 640

hitBonus, missPenalty :: Int
hitBonus = 100
missPenalty = 40

playerPos, scorePos :: V2 Float
playerPos = V2 0 0
scorePos  = V2 xmin (-170)

-- Initialise the game state by creating a player entity
initialize :: System' ()
initialize = do
    playerEntity <- newEntity (Player, MoveDirection Nothing, Position playerPos, Velocity (V2 0 0), Sprite (loadSprite "player.png") (72,24) (Just $ Animation { frameCount = 3, currentFrame = 1, frameSpeed = 0.1 }) )
    generateMap
    return ()

stepPositionFormula :: Float -> Position -> Velocity -> Position
stepPositionFormula dT (Position p) (Velocity v) = Position (p + dT *^ v)

-- Update positions based on velocity and delta time
stepPosition :: Float -> System' ()
stepPosition dT = cmap $ uncurry (stepPositionFormula dT)

-- Lock the player's position within the screen bounds
clampPlayer :: System' ()
clampPlayer = cmap $ \(Player, Position (V2 x y)) -> Position (V2 (min xmax . max xmin $ x) y)

incrementTime :: Float -> System' ()
incrementTime dT = modify global $ \(Time t) -> Time (t + dT)

clearTargets :: System' ()
clearTargets = cmap $ \all@(Target, Position (V2 x _), Velocity _) ->
    if x < xmin || x > xmax
        then Nothing
        else Just all

-- Remove all particles that have expired
stepParticles :: Float -> System' ()
stepParticles dT = cmap $ \(Particle t) ->
    if t < 0
        then Right $ Not @(Particle, Kinetic) -- Not is used to delete the particle
        else Left $ Particle (t - dT)

-- Remove bullets that have gone off the top of the screen, penalizing the player
clearBullets :: System' ()
clearBullets = cmap $ \(Bullet, Position (V2 x y), Score s) ->
    if y > 170
        then Right $ (Not @(Bullet, Kinetic), Score (s - missPenalty)) -- Right of (Not (Bullet, Kinetic), Score) removes the bullet and decrements score
        else Left () -- Left used to do nothing

handleCollisions :: System' ()
handleCollisions = cmapM_ $ \(Target, Position posT, entityT) ->
    cmapM_ $ \(Bullet, Position posB, entityB) ->
        when (norm (posT - posB) < 10) $ do
            -- Remove the target
            destroy entityT (Proxy @(Target, Kinetic))
            -- Remove the bullet
            destroy entityB (Proxy @(Bullet, Kinetic))
            -- Spawn particles
            spawnParticles 15 (Position posB) (-500,500) (200,-50)
            modify global $ \(Score s) -> Score (s + hitBonus)

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

-- TODO: Add boundary box collision check and stop player movement
step :: Float -> System' ()
step dT = do
    incrementTime dT
    stepPosition dT
    stepAnimations dT
    clampPlayer
    blockPlayer
    clearTargets
    clearBullets
    stepParticles dT
    handleCollisions
    triggerEvery dT 0.6 0   $ newEntity (Target, Position (V2 xmin 80), Velocity (V2 enemySpeed 0))
    triggerEvery dT 0.6 0.3 $ newEntity (Target, Position (V2 xmax 120), Velocity (V2 (negate enemySpeed) 0))

handleEvent :: Event -> System' ()
-- Player movement
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) = cmapM_ $ \(Player, Velocity (V2 x y), MoveDirection _, e) -> do
    set e (Velocity (V2 (x - playerSpeed) y))
    set e (MoveDirection $ Just LeftDir)
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _)   = cmapM_ $ \(Player, Velocity (V2 x y), MoveDirection md, e) -> do
    set e (Velocity (V2 (x+playerSpeed) y))
    if md == Just LeftDir
        then set e (MoveDirection Nothing)
        else set e (MoveDirection md)
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) = cmapM_ $ \(Player, Velocity (V2 x y), MoveDirection md, e) -> do
    set e (Velocity (V2 (x+playerSpeed) y))
    if md == Just RightDir
        then set e (MoveDirection Nothing)
        else set e (MoveDirection $ Just RightDir)
handleEvent (EventKey (SpecialKey KeyRight) Up _ _)   = cmapM_ $ \(Player, Velocity (V2 x y), MoveDirection md, e) -> do
    set e (Velocity (V2 (x-playerSpeed) y))
    if md == Just RightDir
        then set e (MoveDirection Nothing)
        else set e (MoveDirection md)
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) = cmapM_ $ \(Player, Velocity (V2 x y), MoveDirection md, e) -> do
        set e (Velocity (V2 x (y+playerSpeed)))
        if md == Just UpDir
            then set e (MoveDirection Nothing)
            else set e (MoveDirection $ Just UpDir)
handleEvent (EventKey (SpecialKey KeyUp) Up _ _)   = cmapM_ $ \(Player, Velocity (V2 x y), MoveDirection md, e) -> do
    set e (Velocity (V2 x (y-playerSpeed)))
    if md == Just UpDir
        then set e (MoveDirection Nothing)
        else set e (MoveDirection md)
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) = cmapM_ $ \(Player, Velocity (V2 x y), MoveDirection md, e) -> do
    set e (Velocity (V2 x (y-playerSpeed)))
    if md == Just DownDir
        then set e (MoveDirection Nothing)
        else set e (MoveDirection $ Just DownDir)
handleEvent (EventKey (SpecialKey KeyDown) Up _ _)   = cmapM_ $ \(Player, Velocity (V2 x y), MoveDirection md, e) -> do
    set e (Velocity (V2 x (y+playerSpeed)))
    if md == Just DownDir
        then set e (MoveDirection Nothing)
        else set e (MoveDirection md)
-- Player shooting
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) = cmapM_ $ \(Player, pos) -> do
    newEntity (Bullet, pos, Velocity (V2 0 bulletSpeed))
    spawnParticles 7 pos (-80,80) (10,100)
-- Exit game
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent _ = return () -- base case

