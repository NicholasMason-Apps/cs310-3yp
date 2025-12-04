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
import qualified SDL
import Control.Exception (handle)

-- Initialise the game state by creating a player entity
initialize :: SDL.WindowConfig -> SDL.Renderer -> System' ()
initialize w r = do
    let spriteList = [
                        (
                            "player-idle",
                            Sprite (64,64) (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = loadSprite r "player/idle.png", looping = True, afterLoopAnimation = Nothing })
                        ),
                        (
                            "player-walk",
                            Sprite (64,64) (Right $ Animation { frameCount = 10, frameSpeed = 0.1, sprites = loadSprite r "player/walk.png", looping = True, afterLoopAnimation = Nothing })
                        ),
                        (
                            "player-knife-attack",
                            Sprite (64,64) (Right $ Animation { frameCount = 9, frameSpeed = 0.1, sprites = loadSprite r "player/knife-attack.png", looping = False, afterLoopAnimation = Just "player-idle" })
                        ),
                        (
                            "player-staff",
                            Sprite (64,64) (Right $ Animation { frameCount = 7, frameSpeed = 0.3, sprites = loadSprite r "player/player-staff.png", looping = False, afterLoopAnimation = Nothing })
                        ),
                        (
                            "player-hit",
                            Sprite (64,64) (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = loadSprite r "player/hit.png", looping = False, afterLoopAnimation = Just "player-idle" })
                        ),
                        (
                            "skeleton-idle",
                            Sprite (64,64) (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = loadSprite r "enemies/skeleton/idle.png", looping = True, afterLoopAnimation = Nothing })
                        ),
                        (
                            "skeleton-walk",
                            Sprite (64,64) (Right $ Animation { frameCount = 10, frameSpeed = 0.1, sprites = loadSprite r "enemies/skeleton/walk.png", looping = True, afterLoopAnimation = Nothing })
                        ),
                        (
                            "skeleton-attack",
                            Sprite (64,64) (Right $ Animation { frameCount = 9, frameSpeed = 0.1, sprites = loadSprite r "enemies/skeleton/attack.png", looping = False, afterLoopAnimation = Just "skeleton-idle" })
                        ),
                        (
                            "skeleton-hit",
                            Sprite (64,64) (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = loadSprite r "enemies/skeleton/hit.png", looping = False, afterLoopAnimation = Just "skeleton-idle" })
                        ),
                        (
                            "skeleton-death",
                            Sprite (64,64) (Right $ Animation { frameCount = 17, frameSpeed = 0.1, sprites = loadSprite r "enemies/skeleton/death.png", looping = False, afterLoopAnimation = Nothing })
                        ),
                        (
                            "reaper-idle",
                            Sprite (64,64) (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = loadSprite r "enemies/reaper/idle.png", looping = True, afterLoopAnimation = Nothing })
                        ),
                        (
                            "reaper-walk",
                            Sprite (64,64) (Right $ Animation { frameCount = 8, frameSpeed = 0.1, sprites = loadSprite r "enemies/reaper/walk.png", looping = True, afterLoopAnimation = Nothing })
                        ),
                        (
                            "reaper-attack",
                            Sprite (64,64) (Right $ Animation { frameCount = 15, frameSpeed = 0.1, sprites = loadSprite r "enemies/reaper/attack.png", looping = False, afterLoopAnimation = Just "reaper-idle" })
                        ),
                        (
                            "reaper-hit",
                            Sprite (64,64) (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = loadSprite r "enemies/reaper/hit.png", looping = False, afterLoopAnimation = Just "reaper-idle" })
                        ),
                        (
                            "reaper-death",
                            Sprite (64,64) (Right $ Animation { frameCount = 15, frameSpeed = 0.1, sprites = loadSprite r "enemies/reaper/death.png", looping = False, afterLoopAnimation = Nothing })
                        ),
                        (
                            "vampire-idle",
                            Sprite (64,64) (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = loadSprite r "enemies/vampire/idle.png", looping = True, afterLoopAnimation = Nothing })
                        ),
                        (
                            "vampire-walk",
                            Sprite (64,64) (Right $ Animation { frameCount = 8, frameSpeed = 0.1, sprites = loadSprite r "enemies/vampire/walk.png", looping = True, afterLoopAnimation = Nothing })
                        ),
                        (
                            "vampire-attack",
                            Sprite (64,64) (Right $ Animation { frameCount = 16, frameSpeed = 0.1, sprites = loadSprite r "enemies/vampire/attack.png", looping = False, afterLoopAnimation = Just "vampire-idle" })
                        ),
                        (
                            "vampire-hit",
                            Sprite (64,64) (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = loadSprite r "enemies/vampire/hit.png", looping = False, afterLoopAnimation = Just "vampire-idle" })
                        ),
                        (
                            "vampire-death",
                            Sprite (64,64) (Right $ Animation { frameCount = 14, frameSpeed = 0.1, sprites = loadSprite r "enemies/vampire/death.png", looping = False, afterLoopAnimation = Nothing })
                        )
                     ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..tileCount], let name = "tile" ++ show n, let path = "tiles/tile" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallTopCount], let name = "wall-top" ++ show n, let path = "tiles/wall-top" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallBottomCount], let name = "wall-bottom" ++ show n, let path = "tiles/wall-bottom" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallLeftCount], let name = "wall-left" ++ show n, let path = "tiles/wall-left" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallRightCount], let name = "wall-right" ++ show n, let path = "tiles/wall-right" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallBottomLeftElbowCount], let name = "wall-bottom-left-elbow" ++ show n, let path = "tiles/wall-bottom-left-elbow" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                     [ (name, Sprite (64,64) (Left pic)) | n <- [1..wallBottomRightElbowCount], let name = "wall-bottom-right-elbow" ++ show n, let path = "tiles/wall-bottom-right-elbow" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                     [
                        ("wall-bottom-right", Sprite (64,64) (Left $ loadSprite r "tiles/wall-bottom-right.png") ),
                        ("wall-bottom-left", Sprite (64,64) (Left $ loadSprite r "tiles/wall-bottom-left.png") ),
                        ("combat-ui", Sprite (1280,720) (Left $ loadSprite r "ui/combat-ui.png") )
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

handlePayload :: [SDL.EventPayload] -> System' ()
handlePayload = mapM_ handleEvent

handleEvent :: SDL.EventPayload -> System' ()
handleEvent (SDL.KeyboardEvent ev) = handleKeyEvent ev
handleEvent _ = return ()

handleKeyEvent :: SDL.KeyboardEventData -> System' ()
handleKeyEvent ev
    | SDL.keyboardEventKeyMotion ev == SDL.Pressed = modify global $ \(KeysPressed ks) -> KeysPressed (Set.insert (SDL.keysymKeycode (SDL.keyboardEventKeysym ev)) ks)
    | SDL.keyboardEventKeyMotion ev == SDL.Released = modify global $ \(KeysPressed ks) -> KeysPressed (Set.delete (SDL.keysymKeycode (SDL.keyboardEventKeysym ev)) ks)
    | otherwise = return ()

-- handleEvent :: Event -> System' ()
-- -- Player movement
-- handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
-- handleEvent (EventKey (SpecialKey k) Down _ _) = modify global $ \(KeysPressed ks) -> KeysPressed (Set.insert (SpecialKey k) ks)
-- handleEvent (EventKey (SpecialKey k) Up _ _) = modify global $ \(KeysPressed ks) -> KeysPressed (Set.delete (SpecialKey k) ks)
-- -- Exit game
-- handleEvent (EventKey (Char 'e') Down _ _) = modify global $ \(KeysPressed ks) -> KeysPressed (Set.insert (Char 'e') ks)
-- handleEvent (EventKey (Char 'e') Up _ _) = modify global $ \(KeysPressed ks) -> KeysPressed (Set.delete (Char 'e') ks)
-- handleEvent (EventResize sz) = set global (Viewport sz)
-- handleEvent _ = return () -- base case