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
    let
        (sref, bbox) = case enemyType enemy of
            Reaper -> (SpriteRef "reaper-idle" (Just 0), BoundaryBox (16, 26) (-1, -12))
            Vampire -> (SpriteRef "vampire-idle" (Just 0), BoundaryBox (16, 30) (-6, -11))
            Skeleton -> (SpriteRef "skeleton-idle" (Just 0), BoundaryBox (24, 26) (-2, -11))
    newEntity (enemy, pos, Velocity (V2 0 0), sref, bbox)