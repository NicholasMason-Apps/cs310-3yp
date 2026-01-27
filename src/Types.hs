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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
import qualified SDL
import qualified Raylib.Types as RL

-- Global stores
newtype RaylibCamera = RaylibCamera RL.Camera3D deriving Show
instance Semigroup RaylibCamera where
    (RaylibCamera c1) <> (RaylibCamera c2) = RaylibCamera c2
instance Monoid RaylibCamera where
    mempty = RaylibCamera (RL.Camera3D (RL.Vector3 0 2 4) (RL.Vector3 0 2 0) (RL.Vector3 0 1 0) 70 RL.CameraPerspective)
instance Component RaylibCamera where type Storage RaylibCamera = Global RaylibCamera  


newtype CameraAngle = CameraAngle (Maybe (Float,Float)) deriving Show
instance Semigroup CameraAngle where
    (CameraAngle ca1) <> (CameraAngle ca2) = CameraAngle ca2
instance Monoid CameraAngle where
    mempty = CameraAngle Nothing
instance Component CameraAngle where type Storage CameraAngle = Global CameraAngle


newtype Viewport = Viewport (Int, Int) deriving Show
instance Semigroup Viewport where
    (Viewport (w1, h1)) <> (Viewport (w2, h2)) = Viewport (w1 + w2, h1 + h2)
instance Monoid Viewport where
    mempty = Viewport (1280, 720)
instance Component Viewport where type Storage Viewport = Global Viewport

data UIState = CombatAttackSelectUI | CombatMagicSelectUI deriving (Show, Eq)
instance Semigroup UIState where
    (<>) :: UIState -> UIState -> UIState
    _ <> u2 = u2
instance Monoid UIState where
    mempty :: UIState
    mempty = CombatAttackSelectUI
instance Component UIState where type Storage UIState = Global UIState

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


-- Input key components and types
data GameKey = GkUp 
            | GkDown 
            | GkLeft 
            | GkRight
            | GkEsc
            | GkE
            | GkSpace
            | GkQ
            deriving (Show, Eq, Ord)

newtype KeyBindings rawKey = KeyBindings (Map.Map rawKey GameKey)

newtype KeysPressed = KeysPressed (Set.Set GameKey)
instance Semigroup KeysPressed where
    (KeysPressed ks1) <> (KeysPressed ks2) = KeysPressed (ks1 <> ks2)
instance Monoid KeysPressed where
    mempty = KeysPressed Set.empty
instance Component KeysPressed where type Storage KeysPressed = Global KeysPressed


newtype SpriteMap = SpriteMap (Map.Map String Sprite)
instance Semigroup SpriteMap where
    (SpriteMap m1) <> (SpriteMap m2) = SpriteMap (m1 `mappend` m2)
instance Monoid SpriteMap where
    mempty = SpriteMap mempty
instance Component SpriteMap where type Storage SpriteMap = Global SpriteMap


newtype Time = Time Float deriving (Show, Num)
instance Semigroup Time where (<>) = (+)
instance Monoid Time where mempty = 0
instance Component Time where type Storage Time = Global Time


data RendererSystem a b c = GlossRenderer a | SDLRenderer b | RaylibRenderer c deriving (Show, Eq)
instance Semigroup (RendererSystem a b c) where
    (<>) :: RendererSystem a b c -> RendererSystem a b c -> RendererSystem a b c
    _ <> r2 = r2
instance Monoid a => Monoid (RendererSystem a b c) where
    mempty :: RendererSystem a b c
    mempty = GlossRenderer mempty
instance Component (RendererSystem a b c) where type Storage (RendererSystem a b c) = Global (RendererSystem a b c)


-- Movement and position components
newtype Position = Position (V2 Float) deriving (Show)
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Float) deriving (Show)
instance Component Velocity where type Storage Velocity = Map Velocity


-- Movement direction component
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


-- Visible components
data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

data EnemyType = Reaper | Vampire | Skeleton | GoldenReaper deriving (Show, Enum)

data Enemy = Enemy { enemyType :: EnemyType } deriving Show
instance Component Enemy where type Storage Enemy = Map Enemy

data Floor = Floor deriving Show
instance Component Floor where type Storage Floor = Map Floor

data Wall = Wall deriving Show
instance Component Wall where type Storage Wall = Map Wall

data Tile = Tile deriving Show
instance Component Tile where type Storage Tile = Map Tile

data Ladder = Ladder deriving Show
instance Component Ladder where type Storage Ladder = Map Ladder

-- BoundaryBox (width, height) (offsetX, offsetY) from centre
data BoundaryBox = BoundaryBox (Int, Int) (Int, Int) deriving (Show)
instance Component BoundaryBox where type Storage BoundaryBox = Map BoundaryBox

data Sprite = Sprite (Int, Int) (RendererSystem (Either Picture Animation) (SDL.Texture, Maybe Animation) (RL.Texture , Maybe Animation))

data SpriteRef = SpriteRef String (Maybe Int) deriving (Show, Eq, Ord)
instance Component SpriteRef where type Storage SpriteRef = Map SpriteRef

data Animation = Animation { frameCount :: Int
                           , frameSpeed :: Float
                           , sprites :: Maybe (V.Vector Picture)
                           , looping :: Bool
                           , afterLoopAnimation :: Maybe String
                           } deriving (Show)


-- Dungeon components
data RoomType = StartRoom | NormalRoom | BossRoom | HubRoom | LadderRoom deriving (Show, Eq)

data GameRoom = GameRoom { roomType :: RoomType,
                           roomLayout :: [[Char]],
                           exits :: [Direction]
                         } deriving (Show)
instance Component GameRoom where type Storage GameRoom = Map GameRoom

newtype Health = Health Int deriving (Show, Num)
instance Component Health where type Storage Health = Map Health

data MapError = MapError deriving (Show)
instance Component MapError where type Storage MapError = Unique MapError 

-- Combat components
newtype CombatEnemy = CombatEnemy Entity deriving (Show)
instance Component CombatEnemy where type Storage CombatEnemy = Unique CombatEnemy

data CombatPlayer = CombatPlayer deriving (Show)
instance Component CombatPlayer where type Storage CombatPlayer = Unique CombatPlayer

data CombatAttackParticle = CombatAttackParticle Entity deriving (Show)
instance Component CombatAttackParticle where type Storage CombatAttackParticle = Unique CombatAttackParticle

data CombatTurn = CombatTurn TurnState deriving (Show, Eq)
instance Semigroup CombatTurn where
    (<>) :: CombatTurn -> CombatTurn -> CombatTurn
    _ <> t2 = t2
instance Monoid CombatTurn where
    mempty :: CombatTurn
    mempty = CombatTurn PlayerTurn
instance Component CombatTurn where type Storage CombatTurn = Global CombatTurn

data TurnState = PlayerTurn | EnemyTurn | PlayerAttacking | EnemyAttacking | PlayerWin deriving (Show, Eq)

data CombatTile = CombatTile deriving (Show)
instance Component CombatTile where type Storage CombatTile = Map CombatTile

data TransitionEvent = ToCombat | ToDungeon | ToNextLevel deriving (Show, Eq)

-- Transition Components
data Transition = Transition {
    trProgress :: Float, -- 0 to 1
    trAngle :: Float,    -- angle in radians
    trSpeed :: Float,
    trCoverEventFired :: Bool,
    trEvent :: TransitionEvent
} deriving (Show)
instance Component Transition where type Storage Transition = Unique Transition

data Particle = Particle Position deriving (Show)
instance Component Particle where type Storage Particle = Map Particle

type FPS = Int

-- Define all the components in the world
makeWorld "World" [''Position,
                    ''Velocity,
                    ''Player,
                    ''Score,
                    ''Time,
                    ''Camera,
                    ''Wall,
                    ''SpriteRef,
                    ''MoveDirection,
                    ''GameRoom,
                    ''Tile,
                    ''Enemy,
                    ''GameState,
                    ''SpriteMap,
                    ''KeysPressed,
                    ''Viewport,
                    ''BoundaryBox,
                    ''CombatEnemy,
                    ''CombatTile,
                    ''Transition,
                    ''CombatPlayer,
                    ''CombatTurn,
                    ''Health,
                    ''UIState,
                    ''MapError,
                    ''Particle,
                    ''CombatAttackParticle,
                    ''Ladder,
                    ''RaylibCamera,
                    ''CameraAngle]

type System' a = System World a
type Kinetic = (Position, Velocity)