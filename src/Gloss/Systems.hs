{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gloss.Systems where

import Apecs
import Apecs.Gloss
import System.Random
import System.Exit
import Linear
import Control.Monad
import Types
import Sprite
import GameMap
import Data.Set (Set)
import qualified Data.Set as Set
import Utils
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (foldl')
import Data.Maybe
import System.IO.Unsafe ( unsafePerformIO )
import qualified Systems as Sys

initialize :: System' ()
initialize = do
    let spriteList = [
                        (
                            "player-idle",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = Just $ loadAnimatedSprite "player/idle.png" 6 (384,64), looping = True, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "player-walk",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 10, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "player/walk.png" 10 (640,64), looping = True, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "player-knife-attack",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 9, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "player/knife-attack.png" 9 (576,64), looping = False, afterLoopAnimation = Just "player-idle" }))
                        ),
                        (
                            "player-staff",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 7, frameSpeed = 0.3, sprites = Just $ loadAnimatedSprite "player/player-staff.png" 7 (448,64), looping = False, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "player-hit",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "player/hit.png" 5 (320,64), looping = False, afterLoopAnimation = Just "player-idle" }))
                        ),
                        (
                            "skeleton-idle",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = Just $ loadAnimatedSprite "enemies/skeleton/idle.png" 6 (384,64), looping = True, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "skeleton-walk",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 10, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/skeleton/walk.png" 10 (640,64), looping = True, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "skeleton-attack",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 9, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/skeleton/attack.png" 9 (576,64), looping = False, afterLoopAnimation = Just "skeleton-idle" }))
                        ),
                        (
                            "skeleton-hit",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/skeleton/hit.png" 5 (320,64), looping = False, afterLoopAnimation = Just "skeleton-idle" }))
                        ),
                        (
                            "skeleton-death",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 17, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/skeleton/death.png" 17 (1088,64), looping = False, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "reaper-idle",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = Just $ loadAnimatedSprite "enemies/reaper/idle.png" 6 (384,64), looping = True, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "reaper-walk",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 8, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/reaper/walk.png" 8 (512,64), looping = True, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "reaper-attack",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 15, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/reaper/attack.png" 15 (960,64), looping = False, afterLoopAnimation = Just "reaper-idle" }))
                        ),
                        (
                            "reaper-hit",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/reaper/hit.png" 5 (320,64), looping = False, afterLoopAnimation = Just "reaper-idle" }))
                        ),
                        (
                            "reaper-death",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 15, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/reaper/death.png" 15 (960,64), looping = False, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "vampire-idle",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = Just $ loadAnimatedSprite "enemies/vampire/idle.png" 6 (384,64), looping = True, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "vampire-walk",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 8, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/vampire/walk.png" 8 (512,64), looping = True, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "vampire-attack",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 16, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/vampire/attack.png" 16 (1024,64), looping = False, afterLoopAnimation = Just "vampire-idle" }))
                        ),
                        (
                            "vampire-hit",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/vampire/hit.png" 5 (320,64), looping = False, afterLoopAnimation = Just "vampire-idle" }))
                        ),
                        (
                            "vampire-death",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 14, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/vampire/death.png" 14 (896,64), looping = False, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "golden-reaper-idle",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 6, frameSpeed = 0.3, sprites = Just $ loadAnimatedSprite "enemies/golden-reaper/idle.png" 6 (384,64), looping = True, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "golden-reaper-walk",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 8, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/golden-reaper/walk.png" 8 (512,64), looping = True, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "golden-reaper-attack",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 15, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/golden-reaper/attack.png" 15 (960,64), looping = False, afterLoopAnimation = Just "golden-reaper-idle" }))
                        ),
                        (
                            "golden-reaper-hit",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/golden-reaper/hit.png" 5 (320,64), looping = False, afterLoopAnimation = Just "golden-reaper-idle" }))
                        ),
                        (
                            "golden-reaper-death",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 15, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "enemies/golden-reaper/death.png" 15 (960,64), looping = False, afterLoopAnimation = Nothing }))
                        )
                     ] ++
                     [ (name, Sprite (64,64) (GlossRenderer $ Left pic)) | n <- [1..tileCount], let name = "tile" ++ show n, let path = "tiles/tile" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (GlossRenderer $ Left pic)) | n <- [1..wallTopCount], let name = "wall-top" ++ show n, let path = "tiles/wall-top" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (GlossRenderer $ Left pic)) | n <- [1..wallBottomCount], let name = "wall-bottom" ++ show n, let path = "tiles/wall-bottom" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (GlossRenderer $ Left pic)) | n <- [1..wallLeftCount], let name = "wall-left" ++ show n, let path = "tiles/wall-left" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (GlossRenderer $ Left pic)) | n <- [1..wallRightCount], let name = "wall-right" ++ show n, let path = "tiles/wall-right" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (GlossRenderer $ Left pic)) | n <- [1..wallBottomLeftElbowCount], let name = "wall-bottom-left-elbow" ++ show n, let path = "tiles/wall-bottom-left-elbow" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [ (name, Sprite (64,64) (GlossRenderer $ Left pic)) | n <- [1..wallBottomRightElbowCount], let name = "wall-bottom-right-elbow" ++ show n, let path = "tiles/wall-bottom-right-elbow" ++ show n ++ ".png", let pic = loadStaticSprite path ] ++
                     [
                        ("wall-bottom-right", Sprite (64,64) (GlossRenderer $ Left $ loadStaticSprite "tiles/wall-bottom-right.png") ),
                        ("wall-bottom-left", Sprite (64,64) (GlossRenderer $ Left $ loadStaticSprite "tiles/wall-bottom-left.png") ),
                        ("combat-ui", Sprite (1280,720) (GlossRenderer $ Left $ loadStaticSprite "ui/combat-ui.png") )
                    ]
    Sys.initialize spriteList

handleEvent :: Event -> System' ()
-- Player movement
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent (EventKey (SpecialKey k) Down _ _) = modify global $ \(KeysPressed ks) -> case ks of
    GlossRenderer ks' -> KeysPressed $ GlossRenderer (Set.insert (SpecialKey k) ks')
    SDLRenderer _ -> let
            ks' = Set.insert (SpecialKey k) Set.empty
        in
            KeysPressed $ GlossRenderer ks'
handleEvent (EventKey (SpecialKey k) Up _ _) = modify global $ \(KeysPressed ks) -> case ks of
    GlossRenderer ks' -> KeysPressed $ GlossRenderer (Set.delete (SpecialKey k) ks')
    SDLRenderer _ -> KeysPressed $ GlossRenderer Set.empty

handleEvent (EventKey (Char 'e') Down _ _) = modify global $ \(KeysPressed ks) -> case ks of
    GlossRenderer ks' -> KeysPressed $ GlossRenderer (Set.insert (Char 'e') ks')
    SDLRenderer _ -> KeysPressed $ GlossRenderer (Set.insert (Char 'e') Set.empty)
handleEvent (EventKey (Char 'e') Up _ _) = modify global $ \(KeysPressed ks) -> case ks of
    GlossRenderer ks' -> KeysPressed $ GlossRenderer (Set.delete (Char 'e') ks')
    SDLRenderer _ -> KeysPressed $ GlossRenderer Set.empty
handleEvent (EventResize sz) = set global (Viewport sz)
handleEvent _ = return () -- base case