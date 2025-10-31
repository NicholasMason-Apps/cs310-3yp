{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameMap ( generateMap ) where

import Apecs
import System.Random
import Linear
import Control.Monad
import Types
import Data.Tree
import Data.List ( maximumBy, minimumBy )
import Data.Ord ( comparing )
import System.Random.Shuffle ( shuffleM )
import Sprite ( loadSprite )

tileSize :: Num a => a
tileSize = 64

roomOffset :: Num a => a
roomOffset = 5 * tileSize

getRoomSize :: [String] -> (Float, Float)
getRoomSize layout = (fromIntegral (length (head layout)) * tileSize, fromIntegral (length layout) * tileSize)

gameRoomLayouts :: [[String]]
gameRoomLayouts = [
    [ "WWWWWDWWWW"
    , "WTTTTTTTTW"
    , "DTTTSTTTTD"
    , "WTTTTTTTTW"
    , "WWWWDWWWWW"
    ],
    [ "WWWWWWWDWWWWWWW"
    , "WTTTTTTTTTTTTTW"
    , "DTTTTTTTTTTTTTD"
    , "WTTTTTTTTTTTTTW"
    , "WWWWWWDWWWWWWWW"
    ],
    [ "____WWWDWWW____"
    , "____WTTTTTW____"
    , "____WTTTTTW____"
    , "WWWWWTTTTTWWWWW"
    , "WTTTTTTTTTTTTTW"
    , "DTTTTTTTTTTTTTD"
    , "WTTTTTTTTTTTTTW"
    , "WWWWWTTTTTWWWWW"
    , "____WTTTTTW____"
    , "____WTTTTTW____"
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
  cmapM_ $ \(gr, Position (V2 grx gry), e) -> do
    let layout = roomLayout gr
        w = length $ head layout
        h = length layout
        adjust dim = if even dim then tileSize / 2 else 0
        offsetX = grx - (fromIntegral w * tileSize / 2) + adjust w + tileSize / 2
        offsetY = gry - (fromIntegral h * tileSize / 2) + adjust h + tileSize / 2
        spriteList = [ (Sprite s (tileSize, tileSize) Nothing, Position (V2 (offsetX + fromIntegral x * tileSize) (offsetY + fromIntegral y * tileSize)), c) 
                        | (y, row) <- zip [0..] layout, (x, c) <- zip [0..] row, c /= '_' && c /= ' ', let s = if c == 'W' then loadSprite "wall.png" else loadSprite "tile.png" ]
    forM_ spriteList $ \(s, p, c) -> do
        case c of
          'W' -> void $ newEntity (Wall, p, s)
          _ -> void $ newEntity (Tile, p, s)
    destroy e (Proxy @Position)
    
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
              newPos = case head $ exits grP of
                          UpDir    -> Position (V2 px (py + ph/2 + rh/2 + roomOffset))
                          DownDir  -> Position (V2 px (py - ph/2 - rh/2 - roomOffset))
                          LeftDir  -> Position (V2 (px - pw/2 - rw/2 - roomOffset) py)
                          RightDir -> Position (V2 (px + pw/2 + rw/2 + roomOffset) py)
          set p grP { exits = drop 1 $ exits grP }
          (finalPos, _) <- cfold (\acc (gr, Position (V2 xGR yGR), entityGR) ->
            if entityGR == p then
              acc
            else
              let (rw', rh') = getRoomSize (roomLayout gr)
                  (Position (V2 nx ny)) = fst acc
                  intersectsX = abs (nx - xGR) < (rw/2 + rw'/2)
                  intersectsY = abs (ny - yGR) < (rh/2 + rh'/2)
              in 
                if intersectsX && intersectsY then
                  let dx = (rw/2 + rw'/2) - abs (nx - xGR)
                      dy = (rh/2 + rh'/2) - abs (ny - yGR)
                      shifts = filter (\(dir, _, _) -> dir /= oppositeDirection (head (exits grP))) [ (UpDir, ceiling (dy / tileSize), Position (V2 nx (ny + fromIntegral (ceiling (dy / tileSize) * tileSize))))
                              , (DownDir, ceiling (dy / tileSize), Position (V2 nx (ny - fromIntegral (ceiling (dy / tileSize) * tileSize))))
                              , (LeftDir, ceiling (dx / tileSize), Position (V2 (nx - fromIntegral (ceiling (dx / tileSize) * tileSize)) ny))
                              , (RightDir, ceiling (dx / tileSize), Position (V2 (nx + fromIntegral (ceiling (dx / tileSize) * tileSize)) ny))
                              ]
                      (_, bestSteps, bestPos) = minimumBy (comparing (\(_, s, _) -> s)) shifts
                      in
                        case snd acc of
                          Nothing -> (bestPos, Just bestSteps)
                          Just s -> if bestSteps < s then (bestPos, Just bestSteps) else acc
                else acc) (newPos, Nothing)
          newEntity (newGr, finalPos)





          -- let (rw, rh) = getRoomSize (roomLayout (roomTypeToGameRoom (rootLabel node) n))
          -- -- For simplicity, place new rooms to the right of the parent room
          -- let newPos = Position (V2 (px + rw + tileSize) py)
          -- let gr = roomTypeToGameRoom (rootLabel node) n
          -- newEntity (gr, newPos)
  -- IDEA - implement a generic bfs which applies a function to each node
  -- make the function it applies a monadic function which checks for intersections across each GameRoom in the entity system currently
  -- and sets the position accordingly, and also inserts it into the map

roomTypeToGameRoom :: RoomType -> Int -> [Direction] -> GameRoom
roomTypeToGameRoom StartRoom _ exits' = GameRoom { roomType = StartRoom, roomLayout = head gameRoomLayouts, exits = exits' }
roomTypeToGameRoom rt n exits' = GameRoom { roomType = rt, roomLayout = gameRoomLayouts !! (n `mod` length gameRoomLayouts), exits = exits' }

generateRandomExitOrder :: SystemT World IO [Direction]
generateRandomExitOrder = liftIO $ shuffleM [UpDir, DownDir, LeftDir, RightDir]

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