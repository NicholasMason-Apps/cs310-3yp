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
import Sprite 
import Data.Maybe ( listToMaybe )
import Data.Foldable ( foldl' )
import Data.Char (intToDigit)
import Graphics.Gloss
import System.IO.Unsafe ( unsafePerformIO )
import Utils
import Data.Maybe ( isNothing )

enemyAggroRange :: Float
enemyAggroRange = 175.0

makeEnemy :: Enemy -> Position -> System' Entity
makeEnemy enemy pos = do
    let
        (sref, bbox) = case enemyType enemy of
            Reaper -> (SpriteRef "reaper-idle" (Just 0), BoundaryBox (16, 26) (-1, -12))
            Vampire -> (SpriteRef "vampire-idle" (Just 0), BoundaryBox (16, 30) (-6, -11))
            Skeleton -> (SpriteRef "skeleton-idle" (Just 0), BoundaryBox (24, 26) (-2, -11))
    n <- liftIO $ randomRIO (25, 45)
    newEntity (enemy, pos, Velocity (V2 0 0), sref, bbox, Health n)

stepEnemyAI :: System' ()
stepEnemyAI = cmapM_ $ \(Player, Position posP) -> do
    cmapM $ \(Enemy _, Position posE, SpriteRef str mn) -> do
        let isInRange = distance posP posE <= enemyAggroRange
        ce <- cfold (\_ (CombatEnemy ce) -> Just ce) Nothing
        if isInRange && isNothing ce then do
            let dir = normalize (posP - posE)
                newVel = dir ^* enemySpeed
                sref' = case str of
                    "reaper-idle" -> SpriteRef "reaper-walk" (Just 0)
                    "vampire-idle" -> SpriteRef "vampire-walk" (Just 0)
                    "skeleton-idle" -> SpriteRef "skeleton-walk" (Just 0)
                    _ -> SpriteRef str mn
            return (Velocity newVel, sref')
        else
            let sref' = case str of
                    "reaper-walk" -> SpriteRef "reaper-idle" (Just 0)
                    "vampire-walk" -> SpriteRef "vampire-idle" (Just 0)
                    "skeleton-walk" -> SpriteRef "skeleton-idle" (Just 0)
                    _ -> SpriteRef str mn
            in
                return (Velocity (V2 0 0), sref')