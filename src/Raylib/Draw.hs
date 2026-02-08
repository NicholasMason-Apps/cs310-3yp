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
import qualified Raylib.Core.Shapes as RL
import qualified Raylib.Types as RL
import qualified Raylib.Util as RL
import qualified Raylib.Util.Colors as RL
import qualified Raylib.Util.RLGL as RL
import qualified Raylib.Core.Textures as RL
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
import Utils
import Control.Monad
import System.Random (randomRIO)

updateCamera :: System' ()
updateCamera = do
    RaylibCamera cam <- get global
    CameraAngle ca <- get global
    gs <- get global
    (V2 mdx mdy) <- liftIO RL.getMouseDelta
    let yaw = maybe 0 fst ca
        pitch = maybe 0 snd ca
        pitch' = clamp (-89,89) (pitch - mdy * 0.3)
        yaw' = yaw - mdx * 0.3
        rp = pitch' * deg2Rad
        ry = yaw' * deg2Rad
    set global $ CameraAngle $ Just (yaw', pitch')
    case gs of
        DungeonState -> cmapM_ $ \(Player, Position (V2 px py)) -> do
            let playerPos = RL.Vector3 px 2.0 (-py)
                forward = RL.Vector3 (cos rp * sin ry) (sin rp) (cos ry * cos rp)
                targetPos = playerPos + forward
            set global $ RaylibCamera $ RL.Camera3D playerPos targetPos (RL.Vector3 0 1 0) 70 RL.CameraPerspective
        CombatState -> cmapM_ $ \CombatPlayer -> do
            let playerPos = RL.Vector3 (-1280/3) 2.0 0
                forward = RL.Vector3 (cos rp * sin ry) (sin rp) (cos ry * cos rp)
                targetPos = playerPos + forward
            set global $ RaylibCamera $ RL.Camera3D playerPos targetPos (RL.Vector3 0 1 0) 70 RL.CameraPerspective
        _ -> return ()

drawBillboard :: SpriteRef -> SpriteMap -> Position -> V2 Bool -> RL.Camera3D -> IO ()
drawBillboard (SpriteRef str Nothing) (SpriteMap smap) pos flip cam = let
        (Sprite _ rs) = smap Map.! str
    in
        case rs of
            RaylibRenderer (t, _) -> RL.drawBillboard cam t (worldTo3D pos) 10 RL.white
            _ -> putStrLn "Error: incorrect renderer used in Raylib rendering system."
drawBillboard (SpriteRef str (Just frameNum)) (SpriteMap smap) pos flip cam = let
        (Sprite (w,h) rs) = smap Map.! str
    in
        case rs of
            RaylibRenderer (t, ma) -> do
                let a = fromMaybe (error "Expected animation data for animated sprite") ma
                    frameWidth = w `div` frameCount a
                    sourceRec = RL.Rectangle (fromIntegral (frameWidth * frameNum)) 0 (fromIntegral frameWidth) (fromIntegral h)
                RL.drawBillboardRec cam t sourceRec (worldTo3D pos) (RL.Vector2 64 64) RL.white
                -- RL.drawBillboard cam t (worldTo3D pos) 10 RL.white
            _ -> putStrLn "Error: incorrect renderer used in Raylib rendering system."

drawTexturedQuad :: SpriteRef -> SpriteMap -> Position -> V2 Bool ->  Set.Set Face -> IO ()
drawTexturedQuad (SpriteRef str Nothing) (SpriteMap smap) pos flip faces = let
        (Sprite (w,h) rs) = smap Map.! str
        pos' = worldTo3D pos
        pos'' = if TopFace `Set.member` faces then pos' + RL.Vector3 0 (-tileSize) 0 else pos'
        pos''' = if BottomFace `Set.member` faces then pos' + RL.Vector3 0 tileSize 0 else pos''
    in
        case rs of
            RaylibRenderer (t, _) -> drawTexturedQuad' t pos''' w h flip RL.white
            _ -> putStrLn "Error: incorrect renderer used in Raylib rendering system."
    where
        drawTexturedQuad' t (RL.Vector3 x y z) w h flip (RL.Color r g b a) = do
            RL.rlSetTexture (RL.texture'id t)
            RL.rlBegin RL.RLQuads
            RL.rlColor4ub r g b a
            let hw = fromIntegral w / 2
                hh = fromIntegral h / 2
                hl = hw * if V2 True False == flip then -1 else 1
                tex u v = RL.rlTexCoord2f u (1 - v) -- Flip V coordinate for correct orientation
            -- Front face
            when (FrontFace `Set.member` faces) $ do
                RL.rlNormal3f 0 0 1
                tex 0 0 >> RL.rlVertex3f (x - hw) (y - hh) (z + hl)
                tex 1 0 >> RL.rlVertex3f (x + hw) (y - hh) (z + hl)
                tex 1 1 >> RL.rlVertex3f (x + hw) (y + hh) (z + hl)
                tex 0 1 >> RL.rlVertex3f (x - hw) (y + hh) (z + hl)
            -- Back face
            when (BackFace `Set.member` faces) $ do
                RL.rlNormal3f 0 0 (-1)
                tex 1 0 >> RL.rlVertex3f (x - hw) (y - hh) (z - hl)
                tex 1 1 >> RL.rlVertex3f (x - hw) (y + hh) (z - hl)
                tex 0 1 >> RL.rlVertex3f (x + hw) (y + hh) (z - hl)
                tex 0 0 >> RL.rlVertex3f (x + hw) (y - hh) (z - hl)
            -- Left face
            when (LeftFace `Set.member` faces) $ do
                RL.rlNormal3f (-1) 0 0
                tex 0 0 >> RL.rlVertex3f (x - hw) (y - hh) (z - hl)
                tex 1 0 >> RL.rlVertex3f (x - hw) (y - hh) (z + hl)
                tex 1 1 >> RL.rlVertex3f (x - hw) (y + hh) (z + hl)
                tex 0 1 >> RL.rlVertex3f (x - hw) (y + hh) (z - hl)
            -- Right face
            when (RightFace `Set.member` faces) $ do
                RL.rlNormal3f 1 0 0
                tex 1 0 >> RL.rlVertex3f (x + hw) (y - hh) (z - hl)
                tex 1 1 >> RL.rlVertex3f (x + hw) (y + hh) (z - hl)
                tex 0 1 >> RL.rlVertex3f (x + hw) (y + hh) (z + hl)
                tex 0 0 >> RL.rlVertex3f (x + hw) (y - hh) (z + hl)
            -- Top face
            when (TopFace `Set.member` faces) $ do
                RL.rlNormal3f 0 1 0
                tex 0 1 >> RL.rlVertex3f (x - hw) (y + hh) (z - hl)
                tex 0 0 >> RL.rlVertex3f (x - hw) (y + hh) (z + hl)
                tex 1 0 >> RL.rlVertex3f (x + hw) (y + hh) (z + hl)
                tex 1 1 >> RL.rlVertex3f (x + hw) (y + hh) (z - hl)
            -- Bottom face
            when (BottomFace `Set.member` faces) $ do
                RL.rlNormal3f 0 (-1) 0
                tex 1 1 >> RL.rlVertex3f (x - hw) (y - hh) (z - hl)
                tex 1 0 >> RL.rlVertex3f (x - hw) (y - hh) (z + hl)
                tex 0 0 >> RL.rlVertex3f (x + hw) (y - hh) (z + hl)
                tex 0 1 >> RL.rlVertex3f (x + hw) (y - hh) (z - hl)

            RL.rlEnd
            RL.rlSetTexture 0

drawTexturedQuad (SpriteRef str (Just frameNum)) (SpriteMap smap) pos flip faces = let
        (Sprite (w,h) rs) = smap Map.! str
        pos' = worldTo3D pos
        pos'' = if TopFace `Set.member` faces then pos' + RL.Vector3 0 (-tileSize) 0 else pos'
        pos''' = if BottomFace `Set.member` faces then pos' + RL.Vector3 0 tileSize 0 else pos''
    in
        case rs of
            RaylibRenderer (t, ma) -> do
                let a = fromMaybe (error "Expected animation data for animated sprite") ma
                    frameWidth = w `div` frameCount a
                    sourceRec = RL.Rectangle (fromIntegral (frameWidth * frameNum)) 0 (fromIntegral frameWidth) (fromIntegral h)
                drawTexturedQuadRec t sourceRec pos''' (fromIntegral frameWidth) (fromIntegral h) flip RL.white
            _ -> putStrLn "Error: incorrect renderer used in Raylib rendering system."
    where
        drawTexturedQuadRec t (RL.Rectangle sx sy sw sh) (RL.Vector3 x y z) w h flip (RL.Color r g b a) = do
            RL.rlSetTexture (RL.texture'id t)
            RL.rlBegin RL.RLQuads
            RL.rlColor4ub r g b a
            let hw = w / 2
                hh = fromIntegral h / 2
                l = w * if V2 True False == flip then -1 else 1
                hl = l / 2
                tex u v = RL.rlTexCoord2f u (1 - v) -- Flip V coordinate for correct orientation
            -- Front face
            when (FrontFace `Set.member` faces) $ do
                RL.rlNormal3f 0 0 1
                RL.rlTexCoord2f (sx / w) ((sy + sh) / fromIntegral h)
                RL.rlVertex3f (x - hw) (y - hh) (z + hl)
                RL.rlTexCoord2f ((sx + sw) / w) ((sy + sh) / fromIntegral h)
                RL.rlVertex3f (x + hw) (y - hh) (z + hl)
                RL.rlTexCoord2f ((sx + sw) / w) (sy / fromIntegral h)
                RL.rlVertex3f (x + hw) (y + hh) (z + hl)
                RL.rlTexCoord2f (sx / w) (sy / fromIntegral h)
                RL.rlVertex3f (x - hw) (y + hh) (z + hl)
            -- Back face
            when (BackFace `Set.member` faces) $ do
                RL.rlNormal3f 0 0 (-1)
                RL.rlTexCoord2f ((sx + sw) / w) ((sy + sh) / fromIntegral h)
                RL.rlVertex3f (x - hw) (y - hh) (z - hl)
                RL.rlTexCoord2f ((sx + sw) / w) (sy / fromIntegral h)
                RL.rlVertex3f (x - hw) (y + hh) (z - hl)
                RL.rlTexCoord2f (sx / w) (sy / fromIntegral h)
                RL.rlVertex3f (x + hw) (y + hh) (z - hl)
                RL.rlTexCoord2f (sx / w) ((sy + sh) / fromIntegral h)
                RL.rlVertex3f (x + hw) (y - hh) (z - hl)
            -- Left face
            when (LeftFace `Set.member` faces) $ do
                RL.rlNormal3f (-1) 0 0
                RL.rlTexCoord2f (sx / w) ((sy + sh) / fromIntegral h)
                RL.rlVertex3f (x - hw) (y - hh) (z - hl)
                RL.rlTexCoord2f ((sx + sw) / w) ((sy + sh) / fromIntegral h)
                RL.rlVertex3f (x - hw) (y - hh) (z + hl)
                RL.rlTexCoord2f ((sx + sw) / w) (sy / fromIntegral h)
                RL.rlVertex3f (x - hw) (y + hh) (z + hl)
                RL.rlTexCoord2f (sx / w) (sy / fromIntegral h)
                RL.rlVertex3f (x - hw) (y + hh) (z - hl)
            -- Right face
            when (RightFace `Set.member` faces) $ do
                RL.rlNormal3f 1 0 0
                RL.rlTexCoord2f ((sx + sw) / w) ((sy + sh) / fromIntegral h)
                RL.rlVertex3f (x + hw) (y - hh) (z - hl)
                RL.rlTexCoord2f ((sx + sw) / w) (sy / fromIntegral h)
                RL.rlVertex3f (x + hw) (y + hh) (z - hl)
                RL.rlTexCoord2f (sx / w) (sy / fromIntegral h)
                RL.rlVertex3f (x + hw) (y + hh) (z + hl)
                RL.rlTexCoord2f (sx / w) ((sy + sh) / fromIntegral h)
                RL.rlVertex3f (x + hw) (y - hh) (z + hl)
            -- Top face
            when (TopFace `Set.member` faces) $ do
                RL.rlNormal3f 0 1 0
                RL.rlTexCoord2f (sx / w) (sy / fromIntegral h)
                RL.rlVertex3f (x - hw) (y + hh) (z - hl)
                RL.rlTexCoord2f (sx / w) ((sy + sh) / fromIntegral h)
                RL.rlVertex3f (x - hw) (y + hh) (z + hl)
                RL.rlTexCoord2f ((sx + sw) / w) ((sy + sh) / fromIntegral h)
                RL.rlVertex3f (x + hw) (y + hh) (z + hl)
                RL.rlTexCoord2f ((sx + sw) / w) (sy / fromIntegral h)
                RL.rlVertex3f (x + hw) (y + hh) (z - hl)
            -- Bottom face
            when (BottomFace `Set.member` faces) $ do
                RL.rlNormal3f 0 (-1) 0
                RL.rlTexCoord2f ((sx + sw) / w) (sy / fromIntegral h)
                RL.rlVertex3f (x - hw) (y - hh) (z - hl)
                RL.rlTexCoord2f ((sx + sw) / w) ((sy + sh) / fromIntegral h)
                RL.rlVertex3f (x - hw) (y - hh) (z + hl)
                RL.rlTexCoord2f (sx / w) ((sy + sh) / fromIntegral h)
                RL.rlVertex3f (x + hw) (y - hh) (z + hl)
                RL.rlTexCoord2f (sx / w) (sy / fromIntegral h)
                RL.rlVertex3f (x + hw) (y - hh) (z - hl)
            RL.rlEnd
            RL.rlSetTexture 0

drawTexture :: SpriteRef -> SpriteMap -> Position -> IO ()
drawTexture (SpriteRef str Nothing) (SpriteMap smap) (Position (V2 x y)) = let
        (Sprite _ rs) = smap Map.! str
    in
        case rs of
            RaylibRenderer (t, _) -> RL.drawTexture t (round x) (round y) RL.white
            _ -> putStrLn "Error: incorrect renderer used in Raylib rendering system."
drawTexture (SpriteRef str (Just frameNum)) (SpriteMap smap) (Position (V2 x y)) = let
        (Sprite (w,h) rs) = smap Map.! str
    in
        case rs of
            RaylibRenderer (t, ma) -> do
                let a = fromMaybe (error "Expected animation data for animated sprite") ma
                    frameWidth = w `div` frameCount a
                    sourceRec = RL.Rectangle (fromIntegral (frameWidth * frameNum)) 0 (fromIntegral frameWidth) (fromIntegral h)
                RL.drawTextureRec t sourceRec (RL.Vector2 (fromIntegral (round x)) (fromIntegral (round y))) RL.white
            _ -> putStrLn "Error: incorrect renderer used in Raylib rendering system."

drawTransition :: System' ()
drawTransition = do
    SpriteMap smap <- get global :: System' SpriteMap
    cmapM_ $ \(Transition p ang _ _ _) -> do
        let t = easeInOut (min 1 p)
            dist = Utils.lerp (-2000) 2000 t
            dx = dist * cos ang
            dy = dist * sin ang
            w = 2500
            h = 2500
            rect = RL.Rectangle (dx + 1280/2) (-dy) w h
            origin = RL.Vector2 (w / 2) (h / 2)
            angleDeg = realToFrac (ang * 180/pi)
        liftIO $ RL.drawRectanglePro rect origin angleDeg RL.black

worldTo3D :: Position -> RL.Vector3
worldTo3D (Position (V2 x y)) = RL.Vector3 x 0 (-y)

drawCombat :: System' ()
drawCombat = do
    RaylibCamera cam <- get global
    smap <- get global :: System' SpriteMap
    CombatTurn turn <- get global
    uiState <- get global :: System' UIState
    liftIO $ do
        RL.beginMode3D cam
    cmapM_ $ \(CombatEnemy _, Position pos, sref) -> liftIO $ drawBillboard sref smap (Position (V2 (1280/3) 0 - V2 (1280/2) 0)) (V2 False False) cam
    cmapM_ $ \(Particle _, Position pos, sref) -> liftIO $ drawBillboard sref smap (Position (pos - V2 (1280/2) 0)) (V2 False False) cam
    liftIO $ do
        RL.endMode3D
    when (turn == PlayerTurn) $ liftIO $ case uiState of
        CombatAttackSelectUI -> drawTexture (SpriteRef "combat-attack-select-ui" Nothing) smap (Position (V2 0 0))
        CombatMagicSelectUI -> drawTexture (SpriteRef "combat-magic-select-ui" Nothing) smap (Position (V2 0 0))


drawDungeon :: System' ()
drawDungeon = do
    RaylibCamera cam <- get global
    smap <- get global :: System' SpriteMap
    liftIO $ do
        RL.beginMode3D cam
    -- cmapM_ $ \(Player, pos) -> do
    --     liftIO $ RL.drawCube (worldTo3D pos) 32 5 32 RL.yellow
    cmapM_ $ \(Wall, pos, sref) -> do
        liftIO $ drawTexturedQuad sref smap pos (V2 False False) (Set.fromList [FrontFace, BackFace, LeftFace, RightFace])
    cmapM_ $ \(Floor, pos, sref) -> do
        liftIO $ drawTexturedQuad sref smap pos (V2 False False) (Set.fromList [TopFace])
    cmapM_ $ \(Enemy _, pos, sref) -> do
        liftIO $ drawBillboard sref smap pos (V2 False False) cam
    liftIO $ do
        RL.endMode3D

draw :: System' ()
draw = do
    gs <- get global
    liftIO $ do
        RL.beginDrawing
        RL.clearBackground (RL.Color 37 19 26 255)
    case gs of
        DungeonState -> drawDungeon
        CombatState -> drawCombat
        _ -> return ()
    drawTransition
    liftIO $ do
        RL.drawFPS 10 10
        RL.endDrawing