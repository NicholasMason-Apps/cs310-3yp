{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GameMap (  ) where

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

generateMapTree :: IO (Tree RoomType)
generateMapTree = do
    depth <- randomRIO (3, 7)
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
            return $ if r <= 7 then NormalRoom else HubRoom

addBossRoom :: Tree RoomType -> Tree RoomType
addBossRoom t = fst $ addBossRoom' t
    where
        addBossRoom' :: Tree RoomType -> (Tree RoomType, Bool)
        addBossRoom' t
            | null (subForest t) =
                ( t { subForest = [Node { rootLabel = BossRoom, subForest = [] }] }, True )
            | otherwise =
                let (newChildren, added) = foldl
                        (\(acc, done) child ->
                        if done
                            then (acc ++ [child], True)
                            else
                            let (child', addedHere) = addBossRoom' child
                            in (acc ++ [child'], addedHere)
                        )
                        ([], False)
                        (subForest t)
                in (t { subForest = newChildren }, added)