{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Raylib.Draw  where

import Apecs
import qualified Raylib.Core as RL
import qualified Raylib.Core.Camera as RL
import qualified Raylib.Core.Models as RL
import qualified Raylib.Core.Text as RL
import qualified Raylib.Types.Core as RL
import qualified Raylib.Types as RL
import qualified Raylib.Util as RL
import qualified Raylib.Util.Colors as RL
import Types
import Control.Monad.IO.Class (liftIO)
import Data.Set (Set)
import qualified Data.Set as Set
import Linear
import Raylib.Util.Math (deg2Rad)
import Data.Ord (clamp)

updateCamera :: System' ()
updateCamera = do
    RaylibCamera cam <- get global
    CameraAngle ca <- get global
    (V2 mdx mdy) <- liftIO RL.getMouseDelta
    let yaw = maybe 0 fst ca
        pitch = maybe 0 snd ca
        pitch' = clamp (-89,89) (pitch - mdy * 0.3)
        yaw' = yaw - mdx * 0.3
        rp = pitch' * deg2Rad
        ry = yaw' * deg2Rad
    set global $ CameraAngle $ Just (yaw', pitch')
    cmapM_ $ \(Player, Position (V2 px py)) -> do
        let forward = RL.Vector3 (cos rp * sin ry) (sin rp) (cos ry * cos rp)
            playerPos = RL.Vector3 px 2.0 (-py)
            targetPos = playerPos + forward
        set global $ RaylibCamera $ RL.Camera3D playerPos targetPos (RL.Vector3 0 1 0) 70 RL.CameraPerspective

worldTo3D :: Position -> RL.Vector3
worldTo3D (Position (V2 x y)) = RL.Vector3 x 0 (-y)

draw :: System' ()
draw = do
    RaylibCamera cam <- get global
    liftIO $ do
        RL.beginDrawing
        RL.clearBackground (RL.Color 37 19 26 255)
        RL.beginMode3D cam
    cmapM_ $ \(Player, pos) -> do
        liftIO $ RL.drawCube (worldTo3D pos) 32 5 32 RL.yellow
    cmapM_ $ \(Wall, pos) -> do
        liftIO $ RL.drawCube (worldTo3D pos) 64 64 64 RL.darkGray
    liftIO $ do
        RL.endMode3D
        RL.drawFPS 10 10
        RL.endDrawing