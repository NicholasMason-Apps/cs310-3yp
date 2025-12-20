{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module SDL.Systems where

import Types
import qualified Systems as Sys
import Apecs
import qualified SDL
import qualified SDL.Font
import SDL.Image (loadTexture)
import qualified Data.Text as T
import Linear
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.IORef
import Control.Monad ( unless )
import System.Exit (exitSuccess)
import System.IO.Unsafe (unsafePerformIO)
import Utils
import Types (RendererSystem(GlossRenderer))


loadSprite :: SDL.Renderer -> FilePath -> SDL.Texture
loadSprite r path = unsafePerformIO $ loadTexture r ("assets/" ++ path)

initialize :: SDL.WindowConfig -> SDL.Renderer -> System' ()
initialize w r = do
    let spriteList = [
                        (
                            "player-idle",
                            Sprite (384,64) (SDLRenderer (loadSprite r "player/idle.png", Just $ Animation { frameCount = 6, frameSpeed = 0.3, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "player-walk",
                            Sprite (640,64) (SDLRenderer (loadSprite r "player/walk.png", Just $ Animation { frameCount = 10, frameSpeed = 0.1, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "player-knife-attack",
                            Sprite (576,64) (SDLRenderer (loadSprite r "player/knife-attack.png", Just $ Animation { frameCount = 9, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "player-idle", sprites = Nothing }))
                        ),
                        (
                            "player-fire-attack",
                            Sprite (704,64) (SDLRenderer (loadSprite r "player/fire-attack.png", Just $ Animation { frameCount = 11, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "player-idle", sprites = Nothing }))
                        ),
                        (
                            "player-electric-attack",
                            Sprite (704,64) (SDLRenderer (loadSprite r "player/electric-attack.png", Just $ Animation { frameCount = 11, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "player-idle", sprites = Nothing }))
                        ),
                        (
                            "player-prismatic-attack",
                            Sprite (704,64) (SDLRenderer (loadSprite r "player/prismatic-attack.png", Just $ Animation { frameCount = 11, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "player-idle", sprites = Nothing }))
                        ),
                        (
                            "player-hit",
                            Sprite (320,64) (SDLRenderer (loadSprite r "player/hit.png", Just $ Animation { frameCount = 5, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "player-idle", sprites = Nothing }))
                        ),
                        (
                            "skeleton-idle",
                            Sprite (384,64) (SDLRenderer (loadSprite r "enemies/skeleton/idle.png", Just $ Animation { frameCount = 6, frameSpeed = 0.3, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "skeleton-walk",
                            Sprite (640,64) (SDLRenderer (loadSprite r "enemies/skeleton/walk.png", Just $ Animation { frameCount = 10, frameSpeed = 0.1, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "skeleton-attack",
                            Sprite (576,64) (SDLRenderer (loadSprite r "enemies/skeleton/attack.png", Just $ Animation { frameCount = 9, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "skeleton-idle", sprites = Nothing }))
                        ),
                        (
                            "skeleton-hit",
                            Sprite (320,64) (SDLRenderer (loadSprite r "enemies/skeleton/hit.png", Just $ Animation { frameCount = 5, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "skeleton-idle", sprites = Nothing }))
                        ),
                        (
                            "skeleton-death",
                            Sprite (1088,64) (SDLRenderer (loadSprite r "enemies/skeleton/death.png", Just $ Animation { frameCount = 17, frameSpeed = 0.1, looping = False, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "reaper-idle",
                            Sprite (384,64) (SDLRenderer (loadSprite r "enemies/reaper/idle.png", Just $ Animation { frameCount = 6, frameSpeed = 0.3, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "reaper-walk",
                            Sprite (512,64) (SDLRenderer (loadSprite r "enemies/reaper/walk.png", Just $ Animation { frameCount = 8, frameSpeed = 0.1, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "reaper-attack",
                            Sprite (960,64) (SDLRenderer (loadSprite r "enemies/reaper/attack.png", Just $ Animation { frameCount = 15, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "reaper-idle", sprites = Nothing }))
                        ),
                        (
                            "reaper-hit",
                            Sprite (320,64) (SDLRenderer (loadSprite r "enemies/reaper/hit.png", Just $ Animation { frameCount = 5, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "reaper-idle", sprites = Nothing }))
                        ),
                        (
                            "reaper-death",
                            Sprite (960,64) (SDLRenderer (loadSprite r "enemies/reaper/death.png", Just $ Animation { frameCount = 15, frameSpeed = 0.1, looping = False, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "vampire-idle",
                            Sprite (384,64) (SDLRenderer (loadSprite r "enemies/vampire/idle.png", Just $ Animation { frameCount = 6, frameSpeed = 0.3, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "vampire-walk",
                            Sprite (512,64) (SDLRenderer (loadSprite r "enemies/vampire/walk.png", Just $ Animation { frameCount = 8, frameSpeed = 0.1, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "vampire-attack",
                            Sprite (1024,64) (SDLRenderer (loadSprite r "enemies/vampire/attack.png", Just $ Animation { frameCount = 16, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "vampire-idle", sprites = Nothing }))
                        ),
                        (
                            "vampire-hit",
                            Sprite (320,64) (SDLRenderer (loadSprite r "enemies/vampire/hit.png", Just $ Animation { frameCount = 5, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "vampire-idle", sprites = Nothing }))
                        ),
                        (
                            "vampire-death",
                            Sprite (896,64) (SDLRenderer (loadSprite r "enemies/vampire/death.png", Just $ Animation { frameCount = 14, frameSpeed = 0.1, looping = False, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "golden-reaper-idle",
                            Sprite (384,64) (SDLRenderer (loadSprite r "enemies/golden-reaper/idle.png", Just $ Animation { frameCount = 6, frameSpeed = 0.3, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "golden-reaper-walk",
                            Sprite (512,64) (SDLRenderer (loadSprite r "enemies/golden-reaper/walk.png", Just $ Animation { frameCount = 8, frameSpeed = 0.1, looping = True, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "golden-reaper-attack",
                            Sprite (960,64) (SDLRenderer (loadSprite r "enemies/golden-reaper/attack.png", Just $ Animation { frameCount = 15, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "golden-reaper-idle", sprites = Nothing }))
                        ),
                        (
                            "golden-reaper-hit",
                            Sprite (320,64) (SDLRenderer (loadSprite r "enemies/golden-reaper/hit.png", Just $ Animation { frameCount = 5, frameSpeed = 0.1, looping = False, afterLoopAnimation = Just "golden-reaper-idle", sprites = Nothing }))
                        ),
                        (
                            "golden-reaper-death",
                            Sprite (960,64) (SDLRenderer (loadSprite r "enemies/golden-reaper/death.png", Just $ Animation { frameCount = 15, frameSpeed = 0.1, looping = False, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "particle-fire",
                            Sprite (7500,100) (SDLRenderer (loadSprite r "particles/fire.png", Just $ Animation { frameCount = 75, frameSpeed = 1/60, looping = False, afterLoopAnimation = Nothing, sprites = Nothing }))
                        ),
                        (
                            "particle-prismatic",
                            Sprite (8100,100) (SDLRenderer (loadSprite r "particles/prismatic.png", Just $ Animation { frameCount = 81, frameSpeed = 1/60, looping = False, afterLoopAnimation = Nothing, sprites = Nothing }))
                        )
                    ] ++
                    [ (name, Sprite (64,64) (SDLRenderer (pic, Nothing))) | n <- [1..tileCount], let name = "tile" ++ show n, let path = "tiles/tile" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                    [ (name, Sprite (64,64) (SDLRenderer (pic, Nothing))) | n <- [1..wallTopCount], let name = "wall-top" ++ show n, let path = "tiles/wall-top" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                    [ (name, Sprite (64,64) (SDLRenderer (pic, Nothing))) | n <- [1..wallBottomCount], let name = "wall-bottom" ++ show n, let path = "tiles/wall-bottom" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                    [ (name, Sprite (64,64) (SDLRenderer (pic, Nothing))) | n <- [1..wallLeftCount], let name = "wall-left" ++ show n, let path = "tiles/wall-left" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                    [ (name, Sprite (64,64) (SDLRenderer (pic, Nothing))) | n <- [1..wallRightCount], let name = "wall-right" ++ show n, let path = "tiles/wall-right" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                    [ (name, Sprite (64,64) (SDLRenderer (pic, Nothing))) | n <- [1..wallBottomLeftElbowCount], let name = "wall-bottom-left-elbow" ++ show n, let path = "tiles/wall-bottom-left-elbow" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                    [ (name, Sprite (64,64) (SDLRenderer (pic, Nothing))) | n <- [1..wallBottomRightElbowCount], let name = "wall-bottom-right-elbow" ++ show n, let path = "tiles/wall-bottom-right-elbow" ++ show n ++ ".png", let pic = loadSprite r path ] ++
                    [
                        ("wall-bottom-right", Sprite (64,64) (SDLRenderer (loadSprite r "tiles/wall-bottom-right.png", Nothing ))),
                        ("wall-bottom-left", Sprite (64,64) (SDLRenderer (loadSprite r "tiles/wall-bottom-left.png", Nothing ))),
                        ("combat-attack-select-ui", Sprite (1280,720) (SDLRenderer (loadSprite r "ui/combat-ui.png", Nothing ))),
                        ("combat-magic-select-ui", Sprite (1280,720) (SDLRenderer (loadSprite r "ui/combat-ui-magic.png", Nothing ))),
                        ("transition", Sprite (2500, 2500) (SDLRenderer (loadSprite r "ui/transition.png", Nothing)))
                    ]
    Sys.initialize spriteList

handlePayload :: [SDL.EventPayload] -> System' ()
handlePayload = mapM_ handleEvent

handleEvent :: SDL.EventPayload -> System' ()
handleEvent (SDL.KeyboardEvent ev) = handleKeyEvent ev
handleEvent _ = return ()

handleKeyEvent :: SDL.KeyboardEventData -> System' ()
handleKeyEvent ev
    | SDL.keyboardEventKeyMotion ev == SDL.Pressed = modify global $ \(KeysPressed ks) -> case ks of
        GlossRenderer _ -> let
                ks' = Set.insert (SDL.keysymKeycode (SDL.keyboardEventKeysym ev)) Set.empty
            in
                KeysPressed $ SDLRenderer ks'
        SDLRenderer ks' -> KeysPressed $ SDLRenderer (Set.insert (SDL.keysymKeycode (SDL.keyboardEventKeysym ev)) ks')
    | SDL.keyboardEventKeyMotion ev == SDL.Released = modify global $ \(KeysPressed ks) -> case ks of
        GlossRenderer _ -> KeysPressed $ SDLRenderer Set.empty
        SDLRenderer ks' -> KeysPressed $ SDLRenderer (Set.delete (SDL.keysymKeycode (SDL.keyboardEventKeysym ev)) ks')
    | otherwise = return ()