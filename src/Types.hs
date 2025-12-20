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
{-# HLINT ignore "Use newtype instead of data" #-}

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

-- Global stores
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


newtype KeysPressed = KeysPressed (RendererSystem (Set.Set Key) (Set.Set SDL.Keycode))
-- newtype KeysPressed = KeysPressed (Set.Set Key) deriving Show
instance Semigroup KeysPressed where
    (KeysPressed s1) <> (KeysPressed s2) = case (s1,s2) of
        (GlossRenderer ks1, GlossRenderer ks2) -> KeysPressed (GlossRenderer (ks1 `Set.union` ks2))
        (SDLRenderer ks1, SDLRenderer ks2)     -> KeysPressed (SDLRenderer (ks1 `Set.union` ks2))
        (GlossRenderer ks1, SDLRenderer _)     -> KeysPressed (GlossRenderer ks1)
        (SDLRenderer ks1, GlossRenderer _)     -> KeysPressed (SDLRenderer ks1)
instance Monoid KeysPressed where
    mempty = KeysPressed (GlossRenderer Set.empty)
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


data RendererSystem a b = GlossRenderer a | SDLRenderer b deriving (Show, Eq)
instance Semigroup (RendererSystem a b) where
    (<>) :: RendererSystem a b -> RendererSystem a b -> RendererSystem a b
    _ <> r2 = r2
instance Monoid a => Monoid (RendererSystem a b) where
    mempty :: RendererSystem a b
    mempty = GlossRenderer mempty
instance Component (RendererSystem a b) where type Storage (RendererSystem a b) = Global (RendererSystem a b)


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

-- BoundaryBox (width, height) (offsetX, offsetY) from centre
data BoundaryBox = BoundaryBox (Int, Int) (Int, Int) deriving (Show)
instance Component BoundaryBox where type Storage BoundaryBox = Map BoundaryBox

data Sprite = Sprite (Int, Int) (RendererSystem (Either Picture Animation) (SDL.Texture, Maybe Animation))

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

-- Transition Components
data Transition = Transition {
    trProgress :: Float, -- 0 to 1
    trAngle :: Float,    -- angle in radians
    trSpeed :: Float,
    trCoverEventFired :: Bool
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
                    ''CombatAttackParticle]

type System' a = System World a
type Kinetic = (Position, Velocity)