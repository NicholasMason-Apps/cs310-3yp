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
import qualified Raylib.Core.Camera as RL
import qualified Raylib.Types as RL
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
import Menu
import Settings
import qualified Data.Vector as V
import Data.Functor
import Data.Foldable
import Data.Aeson
import qualified Data.ByteString.Lazy as BL

-- Initialise the game state by creating a player entity
initialize :: [(String, Sprite)] -> System' ()
initialize spriteList = do
    set global (SpriteMap $ Map.fromList spriteList)
    settings <- liftIO (BL.readFile "settings.json" <&> decode) :: System' (Maybe Settings)
    liftIO $ print settings
    forM_ settings (set global)
    windowedButton <- newEntity (SettingsUIElement, Button WindowedButton, Position (V2 (-132) 124), SpriteRef "windowed-button" Nothing)
    fullscreenButton <- newEntity (SettingsUIElement, Button FullscreenButton, Position (V2 135 124), SpriteRef "fullscreen-button" Nothing)
    _ <- newEntity (SettingsUIElement, ButtonGroup (V.fromList [windowedButton, fullscreenButton]) windowedButton)
    _ <- newEntity (MainMenuUIElement, Button StartGameButton, Position (V2 0 (-150)), SpriteRef "start-game-button" Nothing)
    void $ newEntity (MainMenuUIElement, Button SettingsButton, Position (V2 0 (-230)), SpriteRef "settings-button" Nothing)

incrementTime :: Float -> System' ()
incrementTime dT = do
    modify global $ \(Time t) -> Time (t + dT)
    modify global $ \(ShieldCooldown sc) -> ShieldCooldown (max 0 (sc - dT))

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
    (CameraAngle ca) <- get global :: System' CameraAngle
    case ca of
        Just _ -> cmapM_ $ \(CombatEnemy _, Position (V2 x y)) -> set global $ RaylibCamera $ RL.Camera3D (RL.Vector3 0 0 0) (RL.Vector3 x 0 y) (RL.Vector3 0 1 0) 70 RL.CameraPerspective
        Nothing -> return ()

toNextLevelAction :: System' ()
toNextLevelAction = do
    -- Destroy all Walls, Floors, etc.
    cmapM_ $ \(Wall, e) -> destroy e (Proxy @(Wall, Tile, Position, SpriteRef, BoundaryBox))
    cmapM_ $ \(Ladder, e) -> destroy e (Proxy @(Ladder, Tile, Position, SpriteRef, BoundaryBox))
    cmapM_ $ \(Tile, e) -> destroy e (Proxy @(Tile, Position, SpriteRef))
    cmapM_ $ \(Enemy _, e) -> destroy e (Proxy @(Enemy, Position, Velocity, Health, SpriteRef, BoundaryBox))
    cmapM_ $ \(Heart, Item, e) -> destroy e (Proxy @(Heart, Item, Position, SpriteRef, BoundaryBox))
    cmap $ \(Player, Position _) -> Position playerPos
    generateMap

startDungeonAction :: System' ()
startDungeonAction = do
    _ <- newEntity (Player, Position playerPos, Velocity (V2 0 0), SpriteRef "player-idle" (Just 0),  BoundaryBox (16, 26) (0, -11), Health 100)
    _ <- newEntity (CombatPlayer, Position (V2 (-1280 / 3) 0), SpriteRef "player-idle" (Just 0))
    generateMap
    let offsetX = tileSize / 2 - 1280/2
        offsetY = tileSize / 2 - 720/2
        getTileSprite :: IO String
        getTileSprite = do
            n <- randomRIO (1,tileCount) :: IO Integer
            return $ "tile" ++ show n
        getWallSprite :: IO String
        getWallSprite = do
            n <- randomRIO (1,wallTopCount) :: IO Integer
            return $ "wall-top" ++ show n
    tileList <- liftIO $ sequence [ do
        t <- getTileSprite
        w <- getWallSprite
        let c = if x == -1 || x == ceiling (1280 / tileSize)+1 || y == -1 || y == ceiling (720 / tileSize)+1 then
                    'W'
                else
                    'T'
            sref = if c == 'W' then
                SpriteRef w Nothing
            else
                SpriteRef t Nothing
            pos = Position (V2 (offsetX + fromIntegral x * tileSize) (offsetY + fromIntegral y * tileSize))

        return (sref, pos, c)
        | x <- [-1..ceiling (1280 / tileSize)+1], y <- [-1..ceiling (720 / tileSize)+1] ]
    forM_ tileList $ \(s, p, c) -> do
        case c of
            'W' -> void $ newEntity (CombatWall, p, s)
            _ -> void $ newEntity (CombatTile, p, s)
    set global DungeonState

toMenuAction :: System' ()
toMenuAction = do
    -- Destroy all entities except the transition
    -- Destroy all map entities
    cmapM_ $ \(Wall, e) -> destroy e (Proxy @(Wall, Tile, Position, SpriteRef, BoundaryBox))
    cmapM_ $ \(Ladder, e) -> destroy e (Proxy @(Ladder, Tile, Position, SpriteRef, BoundaryBox))
    cmapM_ $ \(Tile, e) -> destroy e (Proxy @(Tile, Position, SpriteRef))
    cmapM_ $ \(Enemy _, e) -> destroy e (Proxy @(Enemy, Position, Velocity, Health, SpriteRef, BoundaryBox))
    cmapM_ $ \(Heart, Item, e) -> destroy e (Proxy @(Heart, Item, Position, SpriteRef, BoundaryBox))
    -- Destroy player entity
    cmapM_ $ \(Player, e) -> destroy e (Proxy @(Player, Position, Velocity, SpriteRef, BoundaryBox, Health))
    -- Destroy combat entities
    cmapM_ $ \(CombatPlayer, e) -> destroy e (Proxy @(CombatPlayer, Position, SpriteRef))
    cmapM_ $ \(CombatEnemy _, e) -> destroy e (Proxy @(CombatEnemy, Position, SpriteRef))
    set global MenuState

stepTransition :: Float -> System' ()
stepTransition dT = cmapM_ $ \(Transition p ang spd fired event, e) -> do
    let p' = p + dT * spd
    when (not fired && p' >= 0.5) $ case event of
        ToCombat -> toCombatAction
        ToDungeon -> toDungeonAction
        ToNextLevel -> toNextLevelAction
        StartDungeon -> startDungeonAction
        ToMenu -> toMenuAction
        ToSettings -> set global SettingsState
    if p' >= 1 then
        destroy e (Proxy @Transition)
    else
        set e Transition { trProgress = p', trAngle = ang, trSpeed = spd, trCoverEventFired = fired || p' >= 0.5, trEvent = event }

stepFloatingText :: Float -> System' ()
stepFloatingText dt = cmapM_ $ \(ft, e) -> if currLifetime ft + dt >= lifetime ft then
        destroy e (Proxy @(FloatingText, Position, TextLabel, Velocity))
    else
        set e $ ft { currLifetime = currLifetime ft + dt }

-- TODO: Add boundary box collision check and stop player movement
step :: Float -> System' ()
step dT = do
    gs <- get global
    incrementTime dT
    stepAnimations dT
    stepParticles dT
    stepTransition dT
    stepFloatingText dT
    mousePos <- get global :: System' MousePosition
    liftIO $ print mousePos
    case gs of
        DungeonState -> stepDungeon dT
        CombatState  -> stepCombat dT
        MenuState -> stepMenu dT
        SettingsState -> stepSettings dT
        _            -> return ()