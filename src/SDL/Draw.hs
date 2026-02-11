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

drawText :: SDL.Renderer -> SDL.Font.Color -> Position -> FontMap -> String -> IO ()
drawText r col (Position pos) (FontMap fm) str = do
    let font = fm Map.! "roboto"
    case font of
        SDLRenderer font' -> do
            (tex, size) <- generateSolidText r font' col str
            SDL.copy r tex Nothing (Just $ round <$> SDL.Rectangle (SDL.P pos) size)
            SDL.destroyTexture tex
        _ -> return ()

-- Render text to screen easily
generateText :: SDL.Renderer -> SDL.Font.Font -> (SDL.Font.Color -> T.Text -> IO SDL.Surface) -> SDL.Font.Color -> String -> IO (SDL.Texture, V2 Float)
generateText r font f col str = do
    let text = T.pack str
    surface <- f col text
    tex <- SDL.createTextureFromSurface r surface
    SDL.freeSurface surface
    (w,h) <- SDL.Font.size font text
    pure (tex, fromIntegral <$> V2 w h)

generateSolidText :: SDL.Renderer -> SDL.Font.Font -> SDL.Font.Color -> String -> IO (SDL.Texture, V2 Float)
generateSolidText r font = generateText r font (SDL.Font.solid font)

-- Draw a sprite given its SpriteRef and position
-- For sprite sheets, use the frame number to determine which part of the sheet to draw by applying a cropping rectangle
drawSprite :: SpriteRef -> SpriteMap -> Position -> (Int,Int) -> V2 Bool -> SDL.Renderer -> IO ()
drawSprite (SpriteRef str Nothing) (SpriteMap smap) (Position pos) (sw, sh) flip r = let
        (Sprite (w,h) rs) = smap Map.! str
        pos' = SDL.Rectangle (SDL.P (floor <$> pos)) (V2 (fromIntegral (w * sw)) (fromIntegral (h * sh)))
    in
        case rs of
            SDLRenderer (t, _) -> SDL.copyEx r t Nothing (Just pos') 0 Nothing flip
            _ -> putStrLn "Error: incorrect renderer used in SDL rendering system."
drawSprite (SpriteRef str (Just frameNum)) (SpriteMap smap) (Position pos) (sw,sh) flip r = let
        (Sprite (w,h) rs) = smap Map.! str
    in
        case rs of
            SDLRenderer (t, ma) -> do
                let a = fromMaybe (error "Expected animation data for animated sprite") ma
                    frameWidth = w `div` frameCount a
                    dstRect = SDL.Rectangle (SDL.P (floor <$> pos)) (V2 (fromIntegral (frameWidth * sw)) (fromIntegral (h * sh)))
                    srcRect = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral (frameNum * frameWidth)) 0)) (SDL.V2 (fromIntegral frameWidth) (fromIntegral h))
                SDL.copyEx r t (Just srcRect) (Just dstRect) 0 Nothing flip
            _ -> putStrLn "Error: incorrect renderer used in SDL rendering system."

worldToScreen :: Position -> Maybe Position -> Position
worldToScreen (Position (V2 x y)) playerPos = case playerPos of
    Just (Position (V2 px py)) -> Position (V2 (x - px + 1280/2 - pw/2) ((-y) + py + 720/2 - ph/2))
    Nothing -> Position (V2 (x + 1280/2 - tileSize / 2) ((-y) + 720/2 - tileSize / 2))
    where
        (pw,ph) = (64,64)

drawTransition :: SDL.Renderer -> FPS -> System' ()
drawTransition r fps = do
    SpriteMap smap <- get global :: System' SpriteMap
    cmapM_ $ \(Transition p ang _ _ _) -> do
        let t = easeInOut (min 1 p)
            dist = Utils.lerp (-2000) 2000 t
            dx = dist * cos ang
            dy = dist * sin ang
            (Sprite (w,h) rs) = smap Map.! "transition"
            (Position pos) = worldToScreen (Position (V2 (dx - fromIntegral w + 1200) (dy + 1200))) Nothing
            pos' = SDL.Rectangle (SDL.P (floor <$> pos)) (V2 (fromIntegral w) (fromIntegral h))
            angleDeg = realToFrac (ang * 180/pi)
        case rs of
            SDLRenderer (t,_) -> liftIO $ SDL.copyEx r t Nothing (Just pos') angleDeg (Just $ SDL.P (V2 (fromIntegral w `div` 2) (fromIntegral h `div` 2))) (V2 False False)
            _ -> liftIO $ putStrLn "Error: incorrect renderer used in SDL rendering system."

getSprite :: SpriteMap -> SpriteRef -> Sprite
getSprite (SpriteMap smap) (SpriteRef sr _) = smap Map.! sr

drawDungeon :: SDL.Renderer -> FPS -> System' ()
drawDungeon r fps = do
    smap <- get global :: System' SpriteMap
    KeysPressed ks <- get global
    playerPos <- cfold (\_ (Player, p) -> Just p) Nothing
    cmapM_ $ \(Tile, pos, sref) -> when (isSpriteInView playerPos (getSprite smap sref) pos) $ liftIO $ drawSprite sref smap (worldToScreen pos playerPos) (1,1) (V2 False False) r
    cmapM_ $ \(Enemy _, pos, sref) -> when (isSpriteInView playerPos (getSprite smap sref) pos) $ liftIO $ drawSprite sref smap (worldToScreen pos playerPos) (1,1) (V2 False False) r
    cmapM_ $ \(Player, pos, sref) -> let
            flip = if GkLeft `Set.member` ks && GkRight `Set.notMember` ks then V2 True False else V2 False False
        in
            liftIO $ drawSprite sref smap (worldToScreen pos playerPos) (1,1) flip r


drawCombat :: SDL.Renderer -> FPS -> System' ()
drawCombat r fps = do
    smap <- get global :: System' SpriteMap
    CombatTurn turn <- get global
    uiState <- get global :: System' UIState
    cmapM_ $ \(CombatTile, pos, sref) -> liftIO $ drawSprite sref smap (worldToScreen pos Nothing) (1,1) (V2 False False) r
    cmapM_ $ \(CombatPlayer, Position pos, sref) -> liftIO $ drawSprite sref smap (worldToScreen (Position (pos + V2 (-32) 32)) Nothing) (2,2) (V2 False False) r
    cmapM_ $ \(CombatEnemy _, Position pos, sref) -> liftIO $ drawSprite sref smap (worldToScreen (Position (pos + V2 (-32) 32)) Nothing) (2,2) (V2 True False) r
    cmapM_ $ \(Particle _, Position pos, sref) -> liftIO $ drawSprite sref smap (worldToScreen (Position (pos + V2 (-16) 16)) Nothing) (1,1) (V2 False False) r
    when (turn == PlayerTurn) $ liftIO $ case uiState of
            CombatAttackSelectUI -> drawSprite (SpriteRef "combat-attack-select-ui" Nothing) smap (Position (V2 0 0)) (1,1) (V2 False False) r
            CombatMagicSelectUI  -> drawSprite (SpriteRef "combat-magic-select-ui" Nothing) smap (Position (V2 0 0)) (1,1) (V2 False False) r

draw :: SDL.Renderer -> FPS -> System' ()
draw r fps = do
    gs <- get global
    fm <- get global :: System' FontMap
    case gs of
        DungeonState -> do
            cmapM_ $ \(Player, Health hp) -> liftIO $ drawText r (SDL.V4 255 255 255 255) (Position (V2 10 10)) fm ("HP: " ++ show hp)
            drawDungeon r fps
        CombatState  -> do
            cmapM_ $ \(Player, Health hp) -> liftIO $ drawText r (SDL.V4 255 255 255 255) (Position (V2 10 10)) fm ("HP: " ++ show hp)
            drawCombat r fps
        _ -> return () -- drawMenuOrPause r fps
    drawTransition r fps