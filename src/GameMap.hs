{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameMap ( generateMapTree ) where

import Apecs
import Apecs.Gloss
import System.Random
import System.Exit
import Linear
import Control.Monad
import Types
import Data.Maybe ( isJust )
import System.IO.Unsafe ( unsafePerformIO )
import Graphics.Gloss
import Codec.Picture
import Data.Tree
import Data.List ( maximumBy )
-- import System.Random.Shuffle ( shuffleM )

tileSize :: Float
tileSize = 64

roomOffset :: Float
roomOffset = 5 * tileSize

getRoomSize :: [String] -> (Float, Float)
getRoomSize layout = (fromIntegral (length (head layout)) * tileSize, fromIntegral (length layout) * tileSize)

gameRoomLayouts :: [[String]]
gameRoomLayouts = [
    [ "WWWWWDWWWW"
    , "W        W"
    , "D   S    D"
    , "W        W"
    , "WWWWDWWWWW"
    ],
    [ "WWWWWWWDWWWWWWW"
    , "W             W"
    , "D             D"
    , "W             W"
    , "WWWWWWDWWWWWWWW"
    ],
    [ "____WWWDWWW____"
    , "____W     W____"
    , "____W     W____"
    , "WWWWW     WWWWW"
    , "W             W"
    , "D             D"
    , "W             W"
    , "WWWWW     WWWWW"
    , "____W     W____"
    , "____W     W____"
    , "____WWWDWWW____"  ]
  ]

generateMapTree :: IO (Tree RoomType)
generateMapTree = do
    depth <- randomRIO (5, 7) :: IO Int
    t <- recursiveGenerate (Node { rootLabel = StartRoom, subForest = [] }) depth hubRoomCount
    return $ addBossRoom t
    where
      hubRoomCount = 2

addRoom :: Tree RoomType -> Tree RoomType -> Tree RoomType
addRoom t rt = Node { rootLabel = rootLabel t, subForest = subForest t ++ [rt] }

recursiveGenerate :: Tree RoomType -> Int -> Int -> IO (Tree RoomType)
recursiveGenerate t 0 _ = case rootLabel t of
    HubRoom -> do
        n <- randomRIO (2, 3) :: IO Int
        foldM (\acc _ -> do
            let newRoom = Node { rootLabel = NormalRoom, subForest = [] }
            child <- recursiveGenerate newRoom 0 0
            return (addRoom acc child)) t [1..n]
    _ -> return t
recursiveGenerate t depth hubRoomCount = do
    newRoom <- randomRoomType
    let newNode = Node { rootLabel = newRoom, subForest = []}
    case rootLabel newNode of
      HubRoom -> do
          if hubRoomCount > 0 then do
            n <- randomRIO (2, 3) :: IO Int
            children <- replicateM n $ recursiveGenerate (Node { rootLabel = NormalRoom, subForest = [] }) (depth - 1) (hubRoomCount - 1)
            let hubNode = newNode { subForest = children }
            return $ addRoom t hubNode
          else do
              child <- recursiveGenerate (Node { rootLabel = NormalRoom, subForest = [] }) (depth - 1) 0
              return $ addRoom t child
      _ -> do
          child <- recursiveGenerate newNode (depth - 1) hubRoomCount
          return $ addRoom t child
    where
        randomRoomType :: IO RoomType
        randomRoomType = do
            r <- randomRIO (1, 10) :: IO Int
            return $ if r <= 6 then NormalRoom else HubRoom

-- Given a Tree, collect into a list the depth of each leaf, and their path
collectLeavesWithDepth :: Tree a -> [(Int, [Int])]
collectLeavesWithDepth = go [] 0
  where
    go path depth node
      | null (subForest node) = [(depth, path)]
      | otherwise = concat [go (path ++ [i]) (depth + 1) child | (i, child) <- zip [0..] (subForest node)]

-- Update a node at a given path
updateAtPath :: [Int] -> (Tree a -> Tree a) -> Tree a -> Tree a
updateAtPath [] f node = f node
updateAtPath (i:is) f node = node { subForest = [ if j == i then updateAtPath is f child else child | (j, child) <- zip [0..] (subForest node) ] }

-- Adds a single boss room at the deepest leaf
addBossRoom :: Tree RoomType -> Tree RoomType
addBossRoom tree =
  let
    leaves = collectLeavesWithDepth tree
    (_, deepestPath) = maximumBy (\(d1, _) (d2, _) -> compare d1 d2) leaves
  in
    updateAtPath deepestPath (\leaf -> leaf { subForest = [Node BossRoom []] }) tree

generateMap :: System' ()
generateMap = do
  tree <- liftIO generateMapTree
  bfsM insertGameRoom tree
  where
    insertGameRoom :: Maybe Entity -> Tree RoomType -> System' Entity
    insertGameRoom parent node = do
      n <- randomRIO (2, length gameRoomLayouts - 1)
      -- TODO: use cfold to check for intersections and adjust position accordingly
      case parent of
        Nothing -> do
          exits' <- generateRandomExitOrder
          let gr = roomTypeToGameRoom (rootLabel node) 1 exits'
          newEntity (gr, Position (V2 0 0))
        Just p -> do
          Position (V2 px py) <- get p
          exits' <- generateRandomExitOrder
          grP <- get p :: System' GameRoom
          let newGr = roomTypeToGameRoom (rootLabel node) n (filter (/= oppositeDirection (head (exits grP))) exits')
              (pw, ph) = getRoomSize (roomLayout grP)
              (rw, rh) = getRoomSize (roomLayout newGr)
              (e:es) = exits grP
              newPos = case e of
                          UpDir    -> Position (V2 px (py + ph/2 + rh/2 + roomOffset))
                          DownDir  -> Position (V2 px (py - ph/2 - rh/2 - roomOffset))
                          LeftDir  -> Position (V2 (px - pw/2 - rw/2 - roomOffset) py)
                          RightDir -> Position (V2 (px + pw/2 + rw/2 + roomOffset) py)
          set p grP { exits = es }
          -- TODO: use cfoldM to check for intersections between other rooms and adjust position
          -- ISSUE: determining which "face" of the intersection of a room is not as simple as player intersection
          -- since rooms can be of varying sizes and therefore intersect in multiple ways
          -- IDEA: first we find what faces the new room is intersecting with (e.g. if we are adding a room to the bottom of the parent room,
          --  then we find whether it is intersecting with the left, right, or top faces of any room) and then for each direction the room is
          --  intersecting with, we find the minimum offset in tileSize increments to move the room out of intersection
          -- NEW IDEA - since we are using BFS, we can guarantee things like not needing to do a nested cmap
          --            we can use the add direction of this child, and the add direction of the parent
          --            to restrict the possible directions to move in
          let newPos = cfold $ (\acc (gr, Position (V2 xGR, yGR), entityGR) -> 
                  if entityGR /= p then
                    let (rw', rh') = getRoomSize (roomLayout gr)
                        (Position (V2 nx ny)) = acc
                        intersectsX = abs (nx - xGR) < (rw/2 + rw'/2)
                        intersectsY = abs (ny - yGR) < (rh/2 + rh'/2)
                    in if intersectsX && intersectsY
                        then case e of
                            UpDir    -> Position (V2 nx (yGR + rh'/2 + rh/2 + roomOffset))
                            DownDir  -> Position (V2 nx (yGR - rh'/2 - rh/2 - roomOffset))
                            LeftDir  -> Position (V2 (xGR - rw'/2 - rw/2 - roomOffset) ny)
                            RightDir -> Position (V2 (xGR + rw'/2 + rw/2 + roomOffset) ny)
                        else acc
                  else acc) newPos



          -- let (rw, rh) = getRoomSize (roomLayout (roomTypeToGameRoom (rootLabel node) n))
          -- -- For simplicity, place new rooms to the right of the parent room
          -- let newPos = Position (V2 (px + rw + tileSize) py)
          -- let gr = roomTypeToGameRoom (rootLabel node) n
          -- newEntity (gr, newPos)
  -- IDEA - implement a generic bfs which applies a function to each node
  -- make the function it applies a monadic function which checks for intersections across each GameRoom in the entity system currently
  -- and sets the position accordingly, and also inserts it into the map

roomTypeToGameRoom :: RoomType -> Int -> [Direction] -> GameRoom
roomTypeToGameRoom StartRoom _ exits = GameRoom { roomType = StartRoom, roomLayout = head gameRoomLayouts, exits = exits }
roomTypeToGameRoom rt n exits = GameRoom { roomType = rt, roomLayout = gameRoomLayouts !! (n `mod` length gameRoomLayouts), exits = exits }

generateRandomExitOrder = shuffleM [UpDir, DownDir, LeftDir, RightDir]

oppositeDirection :: Direction -> Direction
oppositeDirection UpDir = DownDir
oppositeDirection DownDir = UpDir
oppositeDirection LeftDir = RightDir
oppositeDirection RightDir = LeftDir

-- BFS which keeps track of the parent node, and applies a monadic function to each node
-- The monadic function takes Maybe b as the parent node, and Tree a as the current node
-- and returns m b as the result for the current node
-- b is the type of value to be passed down the tree (e.g., Entity)
bfsM :: Monad m => (Maybe b -> Tree a -> m b) -> Tree a -> m ()
bfsM f tree = go Nothing [tree]
  where
    go _ [] = return ()
    go p (n:ns) = do
      n' <- f p n
      go (Just n') (ns ++ subForest n)