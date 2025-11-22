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
import qualified Data.Map as Map

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

data Enemy = Enemy { enemyType :: EnemyType } deriving Show
instance Component Enemy where type Storage Enemy = Map Enemy

data EnemyType = Reaper | Vampire | Skeleton deriving Show

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

data GameState = MenuState | DungeonState | PauseState | CombatState deriving (Show, Eq)
instance Semigroup GameState where
    (<>) :: GameState -> GameState -> GameState
    _ <> gs2 = gs2
instance Monoid GameState where
    mempty :: GameState
    mempty = DungeonState
instance Component GameState where type Storage GameState = Global GameState

newtype KeysPressed = KeysPressed (Set.Set SpecialKey) deriving Show
instance Semigroup KeysPressed where
    (KeysPressed s1) <> (KeysPressed s2) = KeysPressed (Set.union s1 s2)
instance Monoid KeysPressed where
    mempty = KeysPressed Set.empty
instance Component KeysPressed where type Storage KeysPressed = Global KeysPressed

newtype Time = Time Float deriving (Show, Num)
instance Semigroup Time where (<>) = (+)
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time

data Sprite = Sprite (Int, Int) (Either Picture Animation) deriving (Show)

newtype SpriteMap = SpriteMap (Map.Map String Sprite) deriving Show
instance Semigroup SpriteMap where
    (SpriteMap m1) <> (SpriteMap m2) = SpriteMap (m1 `mappend` m2)
instance Monoid SpriteMap where
    mempty = SpriteMap mempty
instance Component SpriteMap where type Storage SpriteMap = Global SpriteMap

data SpriteRef = SpriteRef String (Maybe Int) deriving (Show, Eq, Ord)
instance Component SpriteRef where type Storage SpriteRef = Map SpriteRef

-- extract sprite sheets 

data Animation = Animation { frameCount :: Int
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
                    ''Wall,
                    ''SpriteRef,
                    ''MoveDirection,
                    ''GameRoom,
                    ''Tile,
                    ''Enemy,
                    ''GameState,
                    ''SpriteMap,
                    ''KeysPressed]

type System' a = System World a
type Kinetic = (Position, Velocity)