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
import qualified Raylib.Types.Core as RL
import qualified Raylib.Types as RL
import qualified Raylib.Util as RL
import qualified Raylib.Util.Colors as RL
import Raylib.Draw
import Types
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, when)
import Linear
import Data.Set (Set)
import qualified Data.Set as Set
import Systems
import qualified Gloss.Systems as GS 

initialize :: System' RL.WindowResources
initialize = do
    let camera = RL.Camera3D (RL.Vector3 0 2 4) (RL.Vector3 1 2 0) (RL.Vector3 0 1 0) 70 RL.CameraPerspective
    set global $ RaylibCamera camera
    GS.initialize
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
    step dT
    updateCamera
    draw
    shouldClose <- liftIO RL.windowShouldClose
    unless shouldClose run


handleEvents :: System' ()
handleEvents = do
    isLeft <- liftIO $ RL.isKeyDown RL.KeyLeft
    -- when (isLeft) $ liftIO $ putStrLn "Left key is down"
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