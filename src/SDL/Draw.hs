{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SDL.Draw where

import Types
import Systems hiding (initialize)
import Apecs hiding (($=))
import SDL (($=))
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import qualified Data.Text as T
import Linear
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Control.Monad ( unless, when )
import System.Exit (exitSuccess)
import Data.Maybe (fromMaybe)
import Dungeon
import qualified Data.Set as Set
import Utils
import Foreign.C (CInt(CInt), CFloat (CFloat))


-- Draw a sprite given its SpriteRef and position
-- For sprite sheets, use the frame number to determine which part of the sheet to draw by applying a cropping rectangle
drawSprite :: SpriteRef -> SpriteMap -> Position -> (Int,Int) -> V2 Bool -> SDL.Renderer -> IO ()
drawSprite (SpriteRef str Nothing) (SpriteMap smap) (Position pos) (sw, sh) flip r = let
        (Sprite (w,h) rs) = smap Map.! str
        pos' = SDL.Rectangle (SDL.P (floor <$> pos)) (V2 (fromIntegral (w * sw)) (fromIntegral (h * sh)))
    in
        case rs of
            GlossRenderer _ -> do
                putStrLn "Error: GlossRenderer used in SDL rendering system."
            SDLRenderer (t, _) -> SDL.copyEx r t Nothing (Just pos') 0 Nothing flip
drawSprite (SpriteRef str (Just frameNum)) (SpriteMap smap) (Position pos) (sw,sh) flip r = let
        (Sprite (w,h) rs) = smap Map.! str
    in
        case rs of
            GlossRenderer _ -> do
                putStrLn "Error: GlossRenderer used in SDL rendering system."
            SDLRenderer (t, ma) -> do
                let a = fromMaybe (error "Expected animation data for animated sprite") ma
                    frameWidth = w `div` frameCount a
                    dstRect = SDL.Rectangle (SDL.P (floor <$> pos)) (V2 (fromIntegral (frameWidth * sw)) (fromIntegral (h * sh)))
                    srcRect = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral (frameNum * frameWidth)) 0)) (SDL.V2 (fromIntegral frameWidth) (fromIntegral h))
                SDL.copyEx r t (Just srcRect) (Just dstRect) 0 Nothing flip

worldToScreen :: Position -> Maybe Position -> Position
worldToScreen (Position (V2 x y)) playerPos = case playerPos of
    Just (Position (V2 px py)) -> Position (V2 (x - px + 1280/2 - pw/2) ((-y) + py + 720/2 - ph/2))
    Nothing -> Position (V2 (x + 1280/2 - tileSize / 2) ((-y) + 720/2 - tileSize / 2))
    where
        (pw,ph) = (64,64)

drawTransition :: SDL.Renderer -> FPS -> System' ()
drawTransition r fps = do
    SpriteMap smap <- get global :: System' SpriteMap
    cmapM_ $ \(Transition p ang _ _) -> do
        let t = easeInOut (min 1 p)
            dist = Utils.lerp (-2000) 2000 t
            dx = dist * cos ang
            dy = dist * sin ang
            (Sprite (w,h) rs) = smap Map.! "transition"
            (Position pos) = worldToScreen (Position (V2 (dx - fromIntegral w + 1200) (dy + 1200))) Nothing
            pos' = SDL.Rectangle (SDL.P (floor <$> pos)) (V2 (fromIntegral w) (fromIntegral h))
            angleDeg = realToFrac (ang * 180/pi)
        case rs of
            GlossRenderer _ -> liftIO $ putStrLn "Error: GlossRenderer used in SDL rendering system."
            SDLRenderer (t,_) -> liftIO $ SDL.copyEx r t Nothing (Just pos') angleDeg (Just $ SDL.P (V2 (fromIntegral w `div` 2) (fromIntegral h `div` 2))) (V2 False False)

getSprite :: SpriteMap -> SpriteRef -> Sprite
getSprite (SpriteMap smap) (SpriteRef sr _) = smap Map.! sr

drawDungeon :: SDL.Renderer -> FPS -> System' ()
drawDungeon r fps = do
    smap <- get global :: System' SpriteMap
    KeysPressed rs <- get global
    let ks = case rs of
            GlossRenderer _ -> Set.empty
            SDLRenderer ks' -> ks'
    playerPos <- cfold (\_ (Player, p) -> Just p) Nothing
    cmapM_ $ \(Tile, pos, sref) -> when (isSpriteInView playerPos (getSprite smap sref) pos) $ liftIO $ drawSprite sref smap (worldToScreen pos playerPos) (1,1) (V2 False False) r
    cmapM_ $ \(Wall, pos, sref) -> when (isSpriteInView playerPos (getSprite smap sref) pos) $ liftIO $ drawSprite sref smap (worldToScreen pos playerPos) (1,1) (V2 False False) r
    cmapM_ $ \(Enemy _, pos, sref) -> when (isSpriteInView playerPos (getSprite smap sref) pos) $ liftIO $ drawSprite sref smap (worldToScreen pos playerPos) (1,1) (V2 False False) r
    cmapM_ $ \(Player, pos, sref) -> let
            flip = if SDL.KeycodeLeft `Set.member` ks && SDL.KeycodeRight `Set.notMember` ks then V2 True False else V2 False False
        in
            liftIO $ drawSprite sref smap (worldToScreen pos playerPos) (1,1) flip r
    -- Green boundary boxes
    -- cmapM_ $ \(Position (V2 x y), BoundaryBox (w,h) (ox,oy)) -> liftIO $ do
    --     let
    --         (Position pos) = worldToScreen $ Position (V2 (x + fromIntegral ox) (y + fromIntegral oy))
    --         rect = SDL.Rectangle
    --             (SDL.P (floor <$> pos))
    --             (SDL.V2 (fromIntegral w) (fromIntegral h))
    --     SDL.rendererDrawColor r SDL.$= SDL.V4 0 255 0 255 
    --     SDL.drawRect r (Just rect)


drawCombat :: SDL.Renderer -> FPS -> System' ()
drawCombat r fps = do
    smap <- get global :: System' SpriteMap
    CombatTurn turn <- get global
    uiState <- get global :: System' UIState
    cmapM_ $ \(CombatTile, pos, sref) -> liftIO $ drawSprite sref smap (worldToScreen pos Nothing) (1,1) (V2 False False) r
    cmapM_ $ \(CombatPlayer, Position pos, sref) -> liftIO $ drawSprite sref smap (worldToScreen (Position (pos + V2 (-32) 32)) Nothing) (2,2) (V2 False False) r
    cmapM_ $ \(CombatEnemy _, Position pos, sref) -> liftIO $ drawSprite sref smap (worldToScreen (Position (pos + V2 (-32) 32)) Nothing) (2,2) (V2 True False) r
    cmapM_ $ \(Particle _, Position pos, sref) -> liftIO $ drawSprite sref smap (worldToScreen (Position (pos + V2 (-32) 32)) Nothing) (1,1) (V2 False False) r
    when (turn == PlayerTurn) $ liftIO $ case uiState of
            CombatAttackSelectUI -> drawSprite (SpriteRef "combat-attack-select-ui" Nothing) smap (Position (V2 0 0)) (1,1) (V2 False False) r
            CombatMagicSelectUI  -> drawSprite (SpriteRef "combat-magic-select-ui" Nothing) smap (Position (V2 0 0)) (1,1) (V2 False False) r

draw :: SDL.Renderer -> FPS -> System' ()
draw r fps = do
    gs <- get global
    case gs of
        DungeonState -> drawDungeon r fps
        CombatState  -> drawCombat r fps
        _ -> return () -- drawMenuOrPause r fps
    drawTransition r fps