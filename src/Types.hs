{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Types where

import Apecs
import Apecs.Gloss
import System.Random
import System.Exit
import Linear
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)
import Codec.Picture
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as Set

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

data Floor = Floor deriving Show
instance Component Floor where type Storage Floor = Map Floor

data MoveDirection = MoveDirection (Set.Set Direction) deriving (Show)
instance Component MoveDirection where type Storage MoveDirection = Map MoveDirection

data Direction = UpDir | DownDir | LeftDir | RightDir deriving (Show, Eq, Ord)
instance Enum Direction where
    fromEnum :: Direction -> Int
    fromEnum UpDir = 1
    fromEnum RightDir = 2
    fromEnum DownDir = 3
    fromEnum LeftDir = 4
    toEnum :: Int -> Direction
    toEnum 1 = UpDir
    toEnum 2 = RightDir
    toEnum 3 = DownDir
    toEnum 4 = LeftDir
    toEnum _ = error "Invalid enum value for Direction"

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

data Keys = Keys (Set.Set SpecialKey) deriving Show
instance Semigroup Keys where
    (Keys a) <> (Keys b) = Keys (Set.union a b)
instance Monoid Keys where mempty = Keys Set.empty
instance Component Keys where type Storage Keys = Global Keys

data Sprite = Sprite (Int, Int) (Either Picture Animations) deriving (Show)
instance Component Sprite where type Storage Sprite = Map Sprite

data Animations = Animations {
    idle :: Animation,
    walk :: Animation,
    current :: Animation
} deriving Show

data Animation = Animation { frameCount :: Int
                           , currentFrame :: Int
                           , frameSpeed :: Float
                           , sprites :: V.Vector Picture
                           } deriving (Show)

-- Procedural generation types
data RoomType = StartRoom | NormalRoom | BossRoom | HubRoom deriving (Show, Eq)

data GameRoom = GameRoom { roomType :: RoomType,
                           roomLayout :: [[Char]],
                           exits :: [Direction]
                         } deriving (Show)
instance Component GameRoom where type Storage GameRoom = Map GameRoom

data Tile = Tile deriving Show
instance Component Tile where type Storage Tile = Map Tile

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
                    ''Wall,
                    ''MoveDirection,
                    ''GameRoom,
                    ''Tile,
                    ''Keys]

type System' a = System World a
type Kinetic = (Position, Velocity)