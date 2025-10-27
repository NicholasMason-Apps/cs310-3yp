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
  gameMapTree <- generateMapTree
  

roomTypeToGameRoom :: RoomType -> Int -> GameRoom
roomTypeToGameRoom StartRoom _ = GameRoom { roomType = StartRoom, roomLayout = head gameRoomLayouts }
roomTypeToGameRoom rt n = GameRoom { roomType = rt, roomLayout = gameRoomLayouts !! (n `mod` length gameRoomLayouts) }