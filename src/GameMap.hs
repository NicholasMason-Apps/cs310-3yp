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

generateMapTree :: IO (Tree RoomType)
generateMapTree = do
    depth <- randomRIO (3, 7) :: IO Int
    t <- recursiveGenerate (Node { rootLabel = StartRoom, subForest = [] }) depth
    return $ addBossRoom t

addRoom :: Tree RoomType -> RoomType -> Tree RoomType
addRoom t rt = Node { rootLabel = rootLabel t, subForest = subForest t ++ [Node { rootLabel = rt, subForest = [] }] }

recursiveGenerate :: Tree RoomType -> Int -> IO (Tree RoomType)
recursiveGenerate t 0 = case rootLabel t of
    HubRoom -> do
        n <- randomRIO (2, 4) :: IO Int
        foldM (\acc _ -> do
            return (addRoom acc NormalRoom)) t [1..n]
    _ -> return t
recursiveGenerate t depth = do
    newRoom <- randomRoomType
    newTree <- case newRoom of
            HubRoom -> do
                n <- randomRIO (2, 4) :: IO Int
                foldM (\acc _ -> do
                    addRoom acc <$> randomRoomType) t [1..n]
            _ -> return $ addRoom t newRoom
    recursiveGenerate newTree (depth - 1)
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

convertRoomToGameSpace :: RoomType -> System' ()
convertRoomToGameSpace rt = case rt of
    StartRoom -> return ()
    NormalRoom -> return ()
    HubRoom -> return ()
    BossRoom -> return ()