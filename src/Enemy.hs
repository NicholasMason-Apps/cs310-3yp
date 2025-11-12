{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Enemy where

import Apecs
import System.Random
import Linear
import Control.Monad
import Types
import Data.Tree
import Data.List ( maximumBy, minimumBy )
import Data.Ord ( comparing )
import System.Random.Shuffle ( shuffleM )
import Sprite ( loadAnimatedSprite )
import Data.Maybe ( listToMaybe )
import Data.Foldable ( foldl' )
import Data.Char (intToDigit)
import Graphics.Gloss
import System.IO.Unsafe ( unsafePerformIO )
import Utils

makeEnemy :: Enemy -> Position -> System' Entity
makeEnemy enemy pos = do
    let idleAnim = case enemyType enemy of
            Reaper  -> Animation { frameCount = 6, currentFrame = 1, frameSpeed = 0.3, sprites = loadAnimatedSprite "enemies/reaper/idle.png" 6 (384,64) }
            Vampire  -> Animation { frameCount = 6, currentFrame = 1, frameSpeed = 0.3, sprites = loadAnimatedSprite "enemies/vampire/idle.png" 6 (384,64) }
            Skeleton -> Animation { frameCount = 6, currentFrame = 1, frameSpeed = 0.3, sprites = loadAnimatedSprite "enemies/skeleton-knife/idle.png" 6 (384,64) }
        walkAnim = case enemyType enemy of
            Reaper  -> Animation { frameCount = 8, currentFrame = 1, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/reaper/walk.png" 10 (640,64) }
            Vampire  -> Animation { frameCount = 8, currentFrame = 1, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/vampire/walk.png" 8 (512,64) }
            Skeleton -> Animation { frameCount = 10, currentFrame = 1, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/skeleton-knife/walk.png" 10 (640,64) }
        sprite = Sprite (64,64) (Right (Animations {
            idle = idleAnim,
            walk = walkAnim,
            current = idleAnim
        }))
    newEntity (enemy, pos, Velocity (V2 0 0), sprite)