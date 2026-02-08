{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Raylib.Systems where

import Apecs
import qualified Raylib.Core as RL
import qualified Raylib.Core.Camera as RL
import qualified Raylib.Core.Models as RL
import qualified Raylib.Core.Text as RL
import qualified Raylib.Core.Textures as RL
import qualified Raylib.Types.Core as RL
import qualified Raylib.Types as RL
import qualified Raylib.Util as RL
import qualified Raylib.Util.Colors as RL
import Raylib.Draw
import Types
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, void)
import Linear
import Data.Set (Set)
import qualified Data.Set as Set
import Systems as Sys
import System.IO.Unsafe ( unsafePerformIO )
import Utils

loadSprite :: String -> RL.Texture
loadSprite path = unsafePerformIO $ do
    img <- RL.loadImage ("assets/" ++ path)
    RL.loadTextureFromImage img

spriteList :: [(String, Sprite)]
spriteList = [
        (
            "player-idle",
            Sprite (384,64) (RaylibRenderer (loadSprite "player/idle.png", Just $ Animation { frameCount = 6, frameSpeed = 0.3, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "player-walk",
            Sprite (640,64) (RaylibRenderer (loadSprite "player/walk.png", Just $ Animation { frameCount = 10, frameSpeed = 0.1, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "player-knife-attack",
            Sprite (576,64) (RaylibRenderer (loadSprite "player/knife-attack.png", Just $ Animation { frameCount = 9, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "player-idle", sprites = Nothing }))
        ),
        (
            "player-fire-attack",
            Sprite (704,64) (RaylibRenderer (loadSprite "player/fire-attack.png", Just $ Animation { frameCount = 11, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "player-idle", sprites = Nothing }))
        ),
        (
            "player-electric-attack",
            Sprite (704,64) (RaylibRenderer (loadSprite "player/electric-attack.png", Just $ Animation { frameCount = 11, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "player-idle", sprites = Nothing }))
        ),
        (
            "player-prismatic-attack",
            Sprite (704,64) (RaylibRenderer (loadSprite "player/prismatic-attack.png", Just $ Animation { frameCount = 11, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "player-idle", sprites = Nothing }))
        ),
        (
            "player-hit",
            Sprite (320,64) (RaylibRenderer (loadSprite "player/hit.png", Just $ Animation { frameCount = 5, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "player-idle", sprites = Nothing }))
        ),
        (
            "skeleton-idle",
            Sprite (384,64) (RaylibRenderer (loadSprite "enemies/skeleton/idle.png", Just $ Animation { frameCount = 6, frameSpeed = 0.3, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "skeleton-walk",
            Sprite (640,64) (RaylibRenderer (loadSprite "enemies/skeleton/walk.png", Just $ Animation { frameCount = 10, frameSpeed = 0.1, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "skeleton-attack",
            Sprite (576,64) (RaylibRenderer (loadSprite "enemies/skeleton/attack.png", Just $ Animation { frameCount = 9, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "skeleton-idle", sprites = Nothing }))
        ),
        (
            "skeleton-hit",
            Sprite (320,64) (RaylibRenderer (loadSprite "enemies/skeleton/hit.png", Just $ Animation { frameCount = 5, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "skeleton-idle", sprites = Nothing }))
        ),
        (
            "skeleton-death",
            Sprite (1088,64) (RaylibRenderer (loadSprite "enemies/skeleton/death.png", Just $ Animation { frameCount = 17, frameSpeed = 0.1, looping = False, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "reaper-idle",
            Sprite (384,64) (RaylibRenderer (loadSprite "enemies/reaper/idle.png", Just $ Animation { frameCount = 6, frameSpeed = 0.3, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "reaper-walk",
            Sprite (512,64) (RaylibRenderer (loadSprite "enemies/reaper/walk.png", Just $ Animation { frameCount = 8, frameSpeed = 0.1, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "reaper-attack",
            Sprite (960,64) (RaylibRenderer (loadSprite "enemies/reaper/attack.png", Just $ Animation { frameCount = 15, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "reaper-idle", sprites = Nothing }))
        ),
        (
            "reaper-hit",
            Sprite (320,64) (RaylibRenderer (loadSprite "enemies/reaper/hit.png", Just $ Animation { frameCount = 5, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "reaper-idle", sprites = Nothing }))
        ),
        (
            "reaper-death",
            Sprite (960,64) (RaylibRenderer (loadSprite "enemies/reaper/death.png", Just $ Animation { frameCount = 15, frameSpeed = 0.1, looping = False, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "vampire-idle",
            Sprite (384,64) (RaylibRenderer (loadSprite "enemies/vampire/idle.png", Just $ Animation { frameCount = 6, frameSpeed = 0.3, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "vampire-walk",
            Sprite (512,64) (RaylibRenderer (loadSprite "enemies/vampire/walk.png", Just $ Animation { frameCount = 8, frameSpeed = 0.1, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "vampire-attack",
            Sprite (1024,64) (RaylibRenderer (loadSprite "enemies/vampire/attack.png", Just $ Animation { frameCount = 16, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "vampire-idle", sprites = Nothing }))
        ),
        (
            "vampire-hit",
            Sprite (320,64) (RaylibRenderer (loadSprite "enemies/vampire/hit.png", Just $ Animation { frameCount = 5, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "vampire-idle", sprites = Nothing }))
        ),
        (
            "vampire-death",
            Sprite (896,64) (RaylibRenderer (loadSprite "enemies/vampire/death.png", Just $ Animation { frameCount = 14, frameSpeed = 0.1, looping = False, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "golden-reaper-idle",
            Sprite (384,64) (RaylibRenderer (loadSprite "enemies/golden-reaper/idle.png", Just $ Animation { frameCount = 6, frameSpeed = 0.3, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "golden-reaper-walk",
            Sprite (512,64) (RaylibRenderer (loadSprite "enemies/golden-reaper/walk.png", Just $ Animation { frameCount = 8, frameSpeed = 0.1, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "golden-reaper-attack",
            Sprite (960,64) (RaylibRenderer (loadSprite "enemies/golden-reaper/attack.png", Just $ Animation { frameCount = 15, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "golden-reaper-idle", sprites = Nothing }))
        ),
        (
            "golden-reaper-hit",
            Sprite (320,64) (RaylibRenderer (loadSprite "enemies/golden-reaper/hit.png", Just $ Animation { frameCount = 5, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "golden-reaper-idle", sprites = Nothing }))
        ),
        (
            "golden-reaper-death",
            Sprite (960,64) (RaylibRenderer (loadSprite "enemies/golden-reaper/death.png", Just $ Animation { frameCount = 15, frameSpeed = 0.1, looping = False, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "particle-fire",
            Sprite (7500,100) (RaylibRenderer (loadSprite "particles/fire.png", Just $ Animation { frameCount = 75, frameSpeed = 1/60, looping = False, afterLoopAnimation = Nothing, sprites = Nothing }))
        ),
        (
            "particle-prismatic",
            Sprite (8100,100) (RaylibRenderer (loadSprite "particles/prismatic.png", Just $ Animation { frameCount = 81, frameSpeed = 1/60, looping = False, afterLoopAnimation = Nothing, sprites = Nothing }))
        )
    ] ++
    [ (name, Sprite (64,64) (RaylibRenderer (pic, Nothing))) | n <- [1..tileCount], let name = "tile" ++ show n, let path = "tiles/tile" ++ show n ++ ".png", let pic = loadSprite path ] ++
    [ (name, Sprite (64,64) (RaylibRenderer (pic, Nothing))) | n <- [1..wallTopCount], let name = "wall-top" ++ show n, let path = "tiles/wall-top" ++ show n ++ ".png", let pic = loadSprite path ] ++
    [ (name, Sprite (64,64) (RaylibRenderer (pic, Nothing))) | n <- [1..wallBottomCount], let name = "wall-bottom" ++ show n, let path = "tiles/wall-bottom" ++ show n ++ ".png", let pic = loadSprite path ] ++
    [ (name, Sprite (64,64) (RaylibRenderer (pic, Nothing))) | n <- [1..wallLeftCount], let name = "wall-left" ++ show n, let path = "tiles/wall-left" ++ show n ++ ".png", let pic = loadSprite path ] ++
    [ (name, Sprite (64,64) (RaylibRenderer (pic, Nothing))) | n <- [1..wallRightCount], let name = "wall-right" ++ show n, let path = "tiles/wall-right" ++ show n ++ ".png", let pic = loadSprite path ] ++
    [ (name, Sprite (64,64) (RaylibRenderer (pic, Nothing))) | n <- [1..wallBottomLeftElbowCount], let name = "wall-bottom-left-elbow" ++ show n, let path = "tiles/wall-bottom-left-elbow" ++ show n ++ ".png", let pic = loadSprite path ] ++
    [ (name, Sprite (64,64) (RaylibRenderer (pic, Nothing))) | n <- [1..wallBottomRightElbowCount], let name = "wall-bottom-right-elbow" ++ show n, let path = "tiles/wall-bottom-right-elbow" ++ show n ++ ".png", let pic = loadSprite path ] ++
    [
        ("wall-bottom-right", Sprite (64,64) (RaylibRenderer (loadSprite "tiles/wall-bottom-right.png", Nothing ))),
        ("wall-bottom-left", Sprite (64,64) (RaylibRenderer (loadSprite "tiles/wall-bottom-left.png", Nothing ))),
        ("combat-attack-select-ui", Sprite (1280,720) (RaylibRenderer (loadSprite "ui/combat-ui.png", Nothing ))),
        ("combat-magic-select-ui", Sprite (1280,720) (RaylibRenderer (loadSprite "ui/combat-ui-magic.png", Nothing ))),
        ("transition", Sprite (2500, 2500) (RaylibRenderer (loadSprite "ui/transition.png", Nothing))),
        ("ladder", Sprite (64,64) (RaylibRenderer (loadSprite "tiles/ladder.png", Nothing )))
    ]

initialize :: System' RL.WindowResources
initialize = do
    let camera = RL.Camera3D (RL.Vector3 0 2 4) (RL.Vector3 1 2 0) (RL.Vector3 0 1 0) 70 RL.CameraPerspective
    set global $ RaylibCamera camera
    set global $ CameraAngle $ Just (0,0)
    liftIO $ do
        window <- RL.initWindow 1280 720 "Dungeon Crawler"
        RL.setTargetFPS 60
#if defined(WSL)
        return window
#else
        RL.disableCursor
        return window
#endif

terminate :: RL.WindowResources -> System' ()
terminate window = liftIO $ RL.closeWindow $ Just window

run :: System' ()
run = do
    -- Main game loop
    handleEvents
    dT <- liftIO RL.getFrameTime
    Sys.step dT
    updateCamera
    draw
    shouldClose <- liftIO RL.windowShouldClose
    unless shouldClose run


handleEvents :: System' ()
handleEvents = do
    isLeft <- liftIO $ RL.isKeyDown RL.KeyLeft
    if isLeft then
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkLeft `Set.insert` ks
    else
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkLeft `Set.delete` ks
    isRight <- liftIO $ RL.isKeyDown RL.KeyRight
    if isRight then
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkRight `Set.insert` ks
    else
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkRight `Set.delete` ks
    isUp <- liftIO $ RL.isKeyDown RL.KeyUp
    if isUp then
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkUp `Set.insert` ks
    else
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkUp `Set.delete` ks
    isDown <- liftIO $ RL.isKeyDown RL.KeyDown
    if isDown then
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkDown `Set.insert` ks
    else
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkDown `Set.delete` ks
    isEsc <- liftIO $ RL.isKeyPressed RL.KeyEscape
    if isEsc then
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkEsc `Set.insert` ks
    else
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkEsc `Set.delete` ks
    isE <- liftIO $ RL.isKeyPressed RL.KeyE
    if isE then
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkE `Set.insert` ks
    else
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkE `Set.delete` ks
    isSpace <- liftIO $ RL.isKeyPressed RL.KeySpace
    if isSpace then
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkSpace `Set.insert` ks
    else
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkSpace `Set.delete` ks
    isQ <- liftIO $ RL.isKeyPressed RL.KeyQ
    if isQ then
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkQ `Set.insert` ks
    else
        modify global $ \(KeysPressed ks) -> KeysPressed $ GkQ `Set.delete` ks