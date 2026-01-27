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
import Data.Map (Map)
import qualified Data.Map as Map
import Types
import Control.Monad.IO.Class (liftIO)
import Data.Set (Set)
import qualified Data.Set as Set
import Linear
import Raylib.Util.Math (deg2Rad)
import Data.Ord (clamp)
import Data.Maybe (fromMaybe)

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

drawBillboard :: SpriteRef -> SpriteMap -> Position -> V2 Bool -> RL.Camera3D -> IO ()
drawBillboard (SpriteRef str Nothing) (SpriteMap smap) pos flip cam = let
        (Sprite (w,h) rs) = smap Map.! str
    in
        case rs of
            RaylibRenderer (t, _) -> RL.drawBillboard cam t (worldTo3D pos) 1.0 RL.white
            _ -> putStrLn "Error: incorrect renderer used in Raylib rendering system."
drawBillboard (SpriteRef str (Just frameNum)) (SpriteMap smap) pos flip cam = let
        (Sprite (w,h) rs) = smap Map.! str
    in
        case rs of
            RaylibRenderer (t, ma) -> do
                let a = fromMaybe (error "Expected animation data for animated sprite") ma
                    frameWidth = w `div` frameCount a
                    -- sourceRec = RL.Rectangle 0 0 1280 720
                    sourceRec = RL.Rectangle (fromIntegral frameWidth * fromIntegral frameNum) 0 (fromIntegral frameWidth) (fromIntegral h)
                RL.drawBillboardRec cam t sourceRec (worldTo3D pos) (RL.Vector2 10 10) RL.white
            _ -> putStrLn "Error: incorrect renderer used in Raylib rendering system."

-- drawSprite :: SpriteRef -> SpriteMap -> Position -> V2 Bool -> IO ()
-- drawSprite (SpriteRef str Nothing) (SpriteMap smap) pos flip = let
--         (Sprite (w,h) rs) = smap Map.! str
--         pos' = worldTo3D pos
--     in
--         case rs of
--             RaylibRenderer (t, _) -> RL.drawModel t pos' 1 RL.white
--             _ -> putStrLn "Error: incorrect renderer used in Raylib rendering system."
-- drawSprite (SpriteRef str Nothing) (SpriteMap smap) pos flip = let
--         (Sprite (w,h) rs) = smap Map.! str
--     in
--         case rs of
--             RaylibRenderer (t, ma) -> do
--                 let a = fromMaybe (error "Expected animation data for animated sprite") ma
--                 frameWidth = w `div` frameCount a

                    

worldTo3D :: Position -> RL.Vector3
worldTo3D (Position (V2 x y)) = RL.Vector3 x 0 (-y)

draw :: System' ()
draw = do
    RaylibCamera cam <- get global
    smap <- get global :: System' SpriteMap
    liftIO $ do
        RL.beginDrawing
        RL.clearBackground (RL.Color 37 19 26 255)
        RL.beginMode3D cam
    cmapM_ $ \(Player, pos) -> do
        liftIO $ RL.drawCube (worldTo3D pos) 32 5 32 RL.yellow
    cmapM_ $ \(Wall, pos) -> do
        liftIO $ RL.drawCube (worldTo3D pos) 64 64 64 RL.darkGray
    cmapM_ $ \(Enemy _, pos, sref) -> do
        liftIO $ drawBillboard sref smap pos (V2 False False) cam

    liftIO $ do
        RL.endMode3D
        RL.drawFPS 10 10
        RL.endDrawing