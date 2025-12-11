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
import Apecs
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import qualified Data.Text as T
import Linear
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Control.Monad ( unless )
import System.Exit (exitSuccess)
import Data.Maybe (fromMaybe)
import Dungeon

-- Draw a sprite given its SpriteRef and position
-- For sprite sheets, use the frame number to determine which part of the sheet to draw by applying a cropping rectangle
drawSprite :: SpriteRef -> SpriteMap -> Position -> SDL.Renderer -> IO ()
drawSprite (SpriteRef str Nothing) (SpriteMap smap) (Position pos) r = let
        (Sprite (w,h) rs) = smap Map.! str
        pos' = SDL.Rectangle (SDL.P (floor <$> pos)) (V2 (fromIntegral w) (fromIntegral h))
    in
        case rs of
            GlossRenderer _ -> do
                putStrLn "Error: GlossRenderer used in SDL rendering system."
            SDLRenderer (t, _) -> SDL.copy r t Nothing (Just pos')
drawSprite (SpriteRef str (Just frameNum)) (SpriteMap smap) (Position pos) r = let
        (Sprite (w,h) rs) = smap Map.! str
    in
        case rs of
            GlossRenderer _ -> do
                putStrLn "Error: GlossRenderer used in SDL rendering system."
            SDLRenderer (t, ma) -> do
                let a = fromMaybe (error "Expected animation data for animated sprite") ma
                    frameWidth = w `div` frameCount a
                    dstRect = SDL.Rectangle (SDL.P (floor <$> pos)) (V2 (fromIntegral frameWidth) (fromIntegral h))
                    srcRect = SDL.Rectangle (SDL.P (SDL.V2 (fromIntegral (frameNum * frameWidth)) 0)) (SDL.V2 (fromIntegral frameWidth) (fromIntegral h))
                SDL.copy r t (Just srcRect) (Just dstRect) 

drawDungeon :: SDL.Renderer -> FPS -> System' ()
drawDungeon r fps = return ()

drawCombat :: SDL.Renderer -> FPS -> System' ()
drawCombat r fps = return ()

draw :: SDL.Renderer -> FPS -> System' ()
draw r fps = do
    gs <- get global
    case gs of
        DungeonState -> drawDungeon r fps
        CombatState  -> drawCombat r fps
        _ -> return () -- drawMenuOrPause r fps