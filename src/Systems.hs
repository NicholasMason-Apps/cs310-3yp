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

-- Initialise the game state by creating a player entity
initialize :: System' ()
initialize = do
    let spriteList = [
                        (
                            "player-idle",
                            Sprite (32,32) (Right $ Animation { frameCount = 7, frameSpeed = 0.3, sprites = loadAnimatedSprite "player/player-idle.png" 7 (224,32) })
                        ),
                        (
                            "player-walk",
                            Sprite (32,32) (Right $ Animation { frameCount = 11, frameSpeed = 0.1, sprites = loadAnimatedSprite "player/player-walk.png" 11 (352,32) })
                        ),
                        (
                            "player-knife",
                            Sprite (32,32) (Right $ Animation { frameCount = 7, frameSpeed = 0.3, sprites = loadAnimatedSprite "player/player-knife.png" 7 (224,32) })
                        ),
                        (
                            "player-staff",
                            Sprite (32,32) (Right $ Animation { frameCount = 7, frameSpeed = 0.3, sprites = loadAnimatedSprite "player/player-staff.png" 7 (224,32) })
                        ),
                        (
                            "skeleton-idle",
                            Sprite (64,64) (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = loadAnimatedSprite "enemies/skeleton/idle.png" 6 (384,64) })
                        ),
                        (
                            "skeleton-walk",
                            Sprite (64,64) (Right $ Animation { frameCount = 10, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/skeleton/walk.png" 10 (640,64) })
                        ),
                        (
                            "reaper-idle",
                            Sprite (64,64) (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = loadAnimatedSprite "enemies/reaper/idle.png" 6 (384,64) })
                        ),
                        (
                            "reaper-walk",
                            Sprite (64,64) (Right $ Animation { frameCount = 8, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/reaper/walk.png" 8 (512,64) })
                        ),
                        (
                            "vampire-idle",
                            Sprite (64,64) (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = loadAnimatedSprite "enemies/vampire/idle.png" 6 (384,64) })
                        ),
                        (
                            "vampire-walk",
                            Sprite (64,64) (Right $ Animation { frameCount = 8, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/vampire/walk.png" 8 (512,64) })
                        )
                     ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..tileCount], let name = "tile" ++ show n, let path = "tiles/tile" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallTopCount], let name = "wall-top" ++ show n, let path = "tiles/wall-top" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallBottomCount], let name = "wall-bottom" ++ show n, let path = "tiles/wall-bottom" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallLeftCount], let name = "wall-left" ++ show n, let path = "tiles/wall-left" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallRightCount], let name = "wall-right" ++ show n, let path = "tiles/wall-right" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallBottomLeftElbowCount], let name = "wall-bottom-left-elbow" ++ show n, let path = "tiles/wall-bottom-left-elbow" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallBottomRightElbowCount], let name = "wall-bottom-right-elbow" ++ show n, let path = "tiles/wall-bottom-right-elbow" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ ("wall-bottom-right", Sprite (64,64) (Left $ loadStaticSprite "tiles/wall-bottom-right.png") ), ("wall-bottom-left", Sprite (64,64) (Left $ loadStaticSprite "tiles/wall-bottom-left.png") ) ]
    set global (SpriteMap $ Map.fromList spriteList)
    playerEntity <- newEntity (Player, Position playerPos, Velocity (V2 0 0), SpriteRef "player-idle" (Just 0), BoundaryBox (18, 26) (-1, 0))
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

-- Update positions based on velocity and delta time
stepPosition :: Float -> System' ()
stepPosition dT = cmap $ uncurry (stepPositionFormula dT)

-- Lock the player's position within the screen bounds
clampPlayer :: System' ()
clampPlayer = cmap $ \(Player, Position (V2 x y)) -> Position (V2 (min xmax . max xmin $ x) y)

incrementTime :: Float -> System' ()
incrementTime dT = modify global $ \(Time t) -> Time (t + dT)

-- clearTargets :: System' ()
-- clearTargets = cmap $ \all@(Target, Position (V2 x _), Velocity _) ->
--     if x < xmin || x > xmax
--         then Nothing
--         else Just all

-- Remove all particles that have expired
stepParticles :: Float -> System' ()
stepParticles dT = cmap $ \(Particle t) ->
    if t < 0
        then Right $ Not @(Particle, Kinetic) -- Not is used to delete the particle
        else Left $ Particle (t - dT)

-- Remove bullets that have gone off the top of the screen, penalizing the player
-- clearBullets :: System' ()
-- clearBullets = cmap $ \(Bullet, Position (V2 x y), Score s) ->
--     if y > 170
--         then Right $ (Not @(Bullet, Kinetic), Score (s - missPenalty)) -- Right of (Not (Bullet, Kinetic), Score) removes the bullet and decrements score
--         else Left () -- Left used to do nothing

-- TODO: add collision between player and enemies
handleEnemyCollisions :: System' ()
handleEnemyCollisions = cmapM_ $ \(Player, Position posP, bbp) -> do
    enemyRes <- cfold (\acc (Enemy _, Position posE, bbE, e) ->
        if checkBoundaryBoxIntersection posP bbp posE bbE && isNothing acc then
            Just e
        else
            acc) Nothing
    case enemyRes of
        Just e -> do
            set global (CombatEnemy e)
            startTransition (pi / 4) 1.0
        Nothing -> return ()


-- handleCollisions = cmapM_ $ \(Target, Position posT, entityT) ->
--     cmapM_ $ \(Bullet, Position posB, entityB) ->
--         when (norm (posT - posB) < 10) $ do
--             -- Remove the target
--             destroy entityT (Proxy @(Target, Kinetic))
--             -- Remove the bullet
--             destroy entityB (Proxy @(Bullet, Kinetic))
--             -- Spawn particles
--             spawnParticles 15 (Position posB) (-500,500) (200,-50)
--             modify global $ \(Score s) -> Score (s + hitBonus)

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

updatePlayerMovement :: System' ()
updatePlayerMovement = do
    KeysPressed ks <- get global
    cmapM_ $ \(Player, Velocity _, SpriteRef sr mn, e) -> do
        let (V2 vx vy) = foldl' (\(V2 ax ay) dir -> case dir of
                                        KeyLeft  -> V2 (ax - playerSpeed) ay
                                        KeyRight -> V2 (ax + playerSpeed) ay
                                        KeyUp    -> V2 ax (ay + playerSpeed)
                                        KeyDown  -> V2 ax (ay - playerSpeed)
                                        _        -> V2 ax ay) (V2 0 0) (Set.toList ks)
            newSprite
              | vx == 0 && vy == 0 && sr /= "player-idle" = SpriteRef "player-idle" (Just 0)
              | (vx /= 0 || vy /= 0) && sr /= "player-walk" = SpriteRef "player-walk" (Just 0)
              | otherwise = SpriteRef sr mn
        set e (Velocity (V2 vx vy))
        set e newSprite

stepDungeon :: Float -> System' ()
stepDungeon dT = do
    updatePlayerMovement
    blockPlayer dT
    stepPosition dT
    handleEnemyCollisions

startTransition :: Float -> Float -> System' ()
startTransition angle speed = do
    cmapM_ $ \(Transition _ _ _ _, e) -> destroy e (Proxy @Transition)
    void $ newEntity (Transition { trProgress = 0, trAngle = angle, trSpeed = speed, trCoverEventFired = False })

stepTransition :: Float -> System' ()
stepTransition dT = cmapM $ \(Transition p ang spd fired, e) -> do
    let p' = p + dT * spd
    when (not fired && p' >= 0.5) $ set global CombatState
    when (p' >= 1) $ destroy e (Proxy @Transition)
    return $ Transition { trProgress = p', trAngle = ang, trSpeed = spd, trCoverEventFired = (fired || p' >= 1) }



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
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) = modify global $ \(KeysPressed ks) -> KeysPressed (Set.insert KeyLeft ks)
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _)   = modify global $ \(KeysPressed ks) -> KeysPressed (Set.delete KeyLeft ks)
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) = modify global $ \(KeysPressed ks) -> KeysPressed (Set.insert KeyRight ks)
handleEvent (EventKey (SpecialKey KeyRight) Up _ _)   = modify global $ \(KeysPressed ks) -> KeysPressed (Set.delete KeyRight ks)
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) = modify global $ \(KeysPressed ks) -> KeysPressed (Set.insert KeyUp ks)
handleEvent (EventKey (SpecialKey KeyUp) Up _ _)   = modify global $ \(KeysPressed ks) -> KeysPressed (Set.delete KeyUp ks)
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) = modify global $ \(KeysPressed ks) -> KeysPressed (Set.insert KeyDown ks)
handleEvent (EventKey (SpecialKey KeyDown) Up _ _)   = modify global $ \(KeysPressed ks) -> KeysPressed (Set.delete KeyDown ks)
-- Exit game
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent (EventResize sz) = set global (Viewport sz)
handleEvent _ = return () -- base case