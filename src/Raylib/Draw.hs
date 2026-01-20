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

updateCamera :: System' ()
updateCamera = do
    let centreX = 640
        centreY = 360
    RaylibCamera cam <- get global
    (V2 mx my) <- liftIO RL.getMousePosition
    (V2 mdx mdy) <- liftIO RL.getMouseDelta
    liftIO $ putStrLn $ "Mouse Position: " ++ show (mx, my)
    liftIO $ putStrLn $ "Mouse Delta: " ++ show (mdx, mdy)
    -- let mdx = mx - fromIntegral centreX
    --     mdy = my - fromIntegral centreY
    cmapM_ $ \(Player, Velocity (V2 vx vy), Position (V2 px py)) -> do
        let pos = RL.Vector3 (vy * 0.001) (vx * 0.001) 0
            rot = RL.Vector3 (mdx*0.3) (mdy*0.3) 0
            -- rot = RL.Vector3 0.1 0 0
        set global $ RaylibCamera $ RL.updateCameraPro cam pos rot 0
        -- cam' <- liftIO $ RL.updateCamera cam RL.CameraModeFirstPerson
        -- set global $ RaylibCamera cam'

draw :: System' ()
draw = do
    RaylibCamera cam <- get global
    liftIO $ do
        RL.beginDrawing
        RL.clearBackground (RL.Color 37 19 26 255)
        RL.beginMode3D cam
        -- Draw entities here
        -- cmapM_ $ \(Position (V2 x y), SpriteRef sref _) -> do
        --     -- Placeholder for drawing sprites
        --     RL.drawCube (RL.Vector3 x y 0) 1 1 1 RL.red
        RL.drawPlane (RL.Vector3 0 0 0) (RL.Vector2 32 32) RL.lightGray -- Draw ground
        RL.drawCube (RL.Vector3 (-16) 2.5 0) 1 5 32 RL.blue -- blue awll
        RL.drawCube (RL.Vector3 16 2.5 0) 1 5 32 RL.lime -- lime wall
        RL.drawCube (RL.Vector3 0 2.5 16) 32 5 1 RL.red -- red wall
        RL.drawCube (RL.Vector3 0 2.5 (-16)) 32 5 1 RL.orange -- orange wall
        RL.endMode3D
        RL.drawFPS 10 10
        RL.endDrawing