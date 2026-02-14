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
import Input

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
                            "player-fire-attack",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 11, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "player/fire-attack.png" 11 (704,64), looping = False, afterLoopAnimation = Just "player-idle" }))
                        ),
                        (
                            "player-electric-attack",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 11, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "player/electric-attack.png" 11 (704,64), looping = False, afterLoopAnimation = Just "player-idle" }))
                        ),
                        (
                            "player-prismatic-attack",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 11, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "player/prismatic-attack.png" 11 (704,64), looping = False, afterLoopAnimation = Just "player-idle" }))
                        ),
                        (
                            "player-hit",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 5, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "player/hit.png" 5 (320,64), looping = False, afterLoopAnimation = Just "player-idle" }))
                        ),
                        (
                            "player-shield",
                            Sprite (64,64) (GlossRenderer (Right $ Animation { frameCount = 6, frameSpeed = 0.1, sprites = Just $ loadAnimatedSprite "player/shield.png" 6 (384,64), looping = False, afterLoopAnimation = Just "player-idle" }))
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
                        ),
                        (
                            "particle-fire",
                            Sprite (100,100) (GlossRenderer (Right $ Animation { frameCount = 75, frameSpeed = 1/60, sprites = Just $ loadAnimatedSprite "particles/fire.png" 75 (7500,100), looping = False, afterLoopAnimation = Nothing }))
                        ),
                        (
                            "particle-prismatic",
                            Sprite (100,100) (GlossRenderer (Right $ Animation { frameCount = 81, frameSpeed = 1/60, sprites = Just $ loadAnimatedSprite "particles/prismatic.png" 81 (8100,100), looping = False, afterLoopAnimation = Nothing }))
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
                        ("combat-attack-select-ui", Sprite (1280,720) (GlossRenderer $ Left $ loadStaticSprite "ui/combat-ui.png") ),
                        ("combat-magic-select-ui", Sprite (1280,720) (GlossRenderer $ Left $ loadStaticSprite "ui/combat-ui-magic.png") ),
                        ("ladder", Sprite (64,64) (GlossRenderer $ Left $ loadStaticSprite "tiles/ladder.png") ),
                        ("heart", Sprite (64,64) (GlossRenderer $ Left $ loadStaticSprite "items/heart.png") ),
                        ("title-screen", Sprite (1280,720) (GlossRenderer $ Left $ loadStaticSprite "ui/title-screen.png") ),
                        ("start-game-button", Sprite (300, 60) (GlossRenderer $ Left $ loadStaticSprite "ui/start-game/button.png") ),
                        ("start-game-button-hover", Sprite (300, 60) (GlossRenderer $ Left $ loadStaticSprite "ui/start-game/hover.png") )
                    ]
    Sys.initialize spriteList

inputBindings :: KeyBindings Key
inputBindings = KeyBindings $ Map.fromList [
        (Char 'w', GkUp),
        (Char 's', GkDown),
        (Char 'a', GkLeft),
        (Char 'd', GkRight),
        (SpecialKey KeySpace, GkSpace),
        (SpecialKey KeyEsc, GkEsc),
        (Char 'e', GkE),
        (Char 'q', GkQ),
        (Char 'f', GkF),
        (MouseButton LeftButton, GkLMB)
    ]

handleEvent :: Event -> System' ()
handleEvent (EventKey k Down _ _) = modify global $ \(KeysPressed ks) -> KeysPressed $ updateKeySet inputBindings k True ks
handleEvent (EventKey k Up _ _) = modify global $ \(KeysPressed ks) -> KeysPressed $ updateKeySet inputBindings k False ks
handleEvent (EventMotion (x,y)) = modify global $ \(MousePosition _) -> MousePosition (V2 x y)
handleEvent _ = return ()