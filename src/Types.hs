{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Apecs
import Apecs.Gloss
import System.Random
import System.Exit
import Linear
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)

newtype Position = Position (V2 Float) deriving (Show)
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving (Show)
instance Component Velocity where type Storage Velocity = Map Velocity

data Target = Target deriving (Show)
instance Component Target where type Storage Target = Map Target

data Bullet = Bullet deriving (Show)
instance Component Bullet where type Storage Bullet = Map Bullet

data Particle = Particle Float deriving (Show)
instance Component Particle where type Storage Particle = Map Particle

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

data Wall = Wall deriving Show
instance Component Wall where type Storage Wall = Map Wall

newtype Score = Score Int deriving (Show, Num)
instance Semigroup Score where (<>) = (+)
instance Monoid Score where mempty = 0
instance Component Score where type Storage Score = Global Score

newtype Time = Time Float deriving (Show, Num)
instance Semigroup Time where (<>) = (+)
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time

newtype FPS = FPS Int deriving (Show, Num)
instance Semigroup FPS where (<>) = (+)
instance Monoid FPS where mempty = 60
instance Component FPS where type Storage FPS = Global FPS

-- Texture coordinates of a sprite
-- StaticSprite for non-animated sprites
-- SpriteSheet for animated sprites
data Sprite = StaticSprite Picture (Int, Int) | SpriteSheet Picture (Int, Int) Int deriving (Show)
instance Component Sprite where type Storage Sprite = Map Sprite

-- Define all the components in the world
makeWorld "World" [''Position,
                    ''Velocity,
                    ''Player, 
                    ''Target, 
                    ''Bullet, 
                    ''Score, 
                    ''Time, 
                    ''Particle, 
                    ''Camera, 
                    ''Sprite,
                    ''Wall]

type System' a = System World a
type Kinetic = (Position, Velocity)