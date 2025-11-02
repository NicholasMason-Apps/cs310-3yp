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
import Data.Maybe ( listToMaybe )
import Data.Foldable ( foldl' )
import Data.Char (intToDigit)

tileSize :: Num a => a
tileSize = 64

roomOffset :: Num a => a
roomOffset = 4 * tileSize

getRoomSize :: [String] -> (Float, Float)
getRoomSize layout = (fromIntegral (length (head layout)) * tileSize, fromIntegral (length layout) * tileSize)

gameRoomLayouts :: [[String]]
gameRoomLayouts = [
    [ "WWWWW1WWWW"
    , "WTTTTTTTT2"
    , "4TTTSTTTTW"
    , "WTTTTTTTTW"
    , "WW3WWWWWWW"
    ],
    [ "WWWWWWW1WWWWWWW"
    , "WTTTTTTTTTTTTTW"
    , "4TTTTTTTTTTTTT2"
    , "WTTTTTTTTTTTTTW"
    , "WWWWWWW3WWWWWWW"
    ],
    [ "____WWW1WWW____"
    , "____WTTTTTW____"
    , "____WTTTTTW____"
    , "WWWWWTTTTTWWWWW"
    , "WTTTTTTTTTTTTTW"
    , "4TTTTTTTTTTTTT2"
    , "WTTTTTTTTTTTTTW"
    , "WWWWWTTTTTWWWWW"
    , "____WTTTTTW____"
    , "____WTTTTTW____"
    , "____WWW3WWW____"  ]
  ]

generateMapTree :: IO (Tree RoomType)
generateMapTree = do
    depth <- randomRIO (5, 7) :: IO Int
    t <- recursiveGenerate (Node { rootLabel = StartRoom, subForest = [] }) 3 hubRoomCount
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
    liftIO $ putStrLn $ "Game Room exits: " ++ show (exits gr)
    let layout = roomLayout gr
        w = length $ head layout
        h = length layout
        tileCheck :: Char -> Bool
        tileCheck c = (c `notElem` " _1234") ||
                      (c == '1' && UpDir `elem` exits gr) ||
                      (c == '2' && RightDir `elem` exits gr) ||
                      (c == '3' && DownDir `elem` exits gr) ||
                      (c == '4' && LeftDir `elem` exits gr)
        halfAdjust v = if even v then tileSize / 2 else 0
        offsetX = grx - (fromIntegral w * tileSize / 2) + halfAdjust w + tileSize / 2
        offsetY = gry - (fromIntegral h * tileSize / 2) + halfAdjust h + tileSize / 2
        spriteList = [ (Sprite s (tileSize, tileSize) Nothing, Position (V2 (offsetX + fromIntegral x * tileSize) (offsetY + fromIntegral (h - 1 - y) * tileSize)), c)
                        | (y, row) <- zip [0..] layout, (x, c) <- zip [0..] row, tileCheck c, let s = if c `elem` "W1234" then loadSprite "wall.png" else loadSprite "tile.png" ]
    forM_ spriteList $ \(s, p, c) -> do
        if c `elem` "W1234" then
          void $ newEntity (Wall, p, s)
          -- _ -> void $ newEntity (Tile, p, s)
        else
          return ()
    destroy e (Proxy @Position)
  where
    insertGameRoom :: Maybe Entity -> Tree RoomType -> System' Entity
    insertGameRoom parent node = do
      n <- randomRIO (1, length gameRoomLayouts - 1)
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
          -- let newGr = roomTypeToGameRoom (rootLabel node) n []
          --     (rw, rh) = getRoomSize (roomLayout newGr)
          --     (cx, cy) = connectionPosition (head (exits grP)) (roomLayout grP)
          --     newPos = Position (V2 (px + cx) (py + cy))
          -- -- (finalPos, _) <- cfold (\acc (gr, Position (V2 xgr ygr), entityGR) ->
          -- --   if entityGR == p then
          -- --     acc
          -- --   else
          -- --     let (rw', rh') = getRoomSize (roomLayout gr)
          -- --         (Position (V2 nx ny)) = fst acc
          -- --         intersectsX = abs (nx - xgr) < (rw/2 + rw'/2)
          -- --         intersectsY = abs (ny - ygr) < (rh/2 + rh'/2)
          -- --     in
          -- --       if intersectsX && intersectsY then

          -- --       else acc) (newPos, Nothing)
          -- Attempt to find the first direction which does not intersect
          let newLayout = getRoomLayout n
          intersections <- mapM (\dir -> checkRoomIntersectionInDirection (Position (V2 px py)) grP dir newLayout) (exits grP)
          let res = foldl' (\acc (intersects, dir) ->
                case acc of
                  Just _ -> acc
                  Nothing -> if not intersects then
                      Just (dir, Position (V2 (px + fst (connectionPosition dir (roomLayout grP) newLayout)) (py + snd (connectionPosition dir (roomLayout grP) newLayout))))
                    else Nothing) Nothing (zip intersections (exits grP))
          case res of
            Just (dir, finalPos) -> do
              let newGr = roomTypeToGameRoom (rootLabel node) n (filter (/= oppositeDirection dir) exits')
              set p (grP { exits = filter (/= dir) (exits grP) })
              newEntity (newGr, finalPos)
            Nothing -> do
              -- If all directions intersect, place it in the first direction by shifting it in that direction until it doesn't intersect
              let dir = head (exits grP)
                  (cx, cy) = connectionPosition dir (roomLayout grP) newLayout
                  distanceToClear dir' (cX, cY) (cW, cH) (oX, oY) (oW, oH) = let
                      raw = case dir' of
                        UpDir -> (oY + oH / 2) - (cY - cH / 2)
                        DownDir -> (cY + cH / 2) - (oY - oH / 2)
                        LeftDir -> (cX + cW / 2) - (oX - oW / 2)
                        RightDir -> (oX + oW / 2) - (cX - cW / 2)
                    in
                      ceiling (raw / tileSize) * tileSize
              (fx,fy) <- cfoldM (\acc (gr, Position (V2 grX grY)) -> do
                  let (rw', rh') = getRoomSize (roomLayout gr)
                      intersectsX = abs ((px + cx + fst acc) - grX) < (fst (getRoomSize newLayout)/2 + rw'/2)
                      intersectsY = abs ((py + cy + snd acc) - grY) < (snd (getRoomSize newLayout)/2 + rh'/2)
                      shiftAmount = fromIntegral $ distanceToClear dir (px + cx + fst acc, py + cy + snd acc) (fst (getRoomSize newLayout), snd (getRoomSize newLayout)) (grX, grY) (rw', rh')
                  if intersectsX && intersectsY then
                      case dir of
                        UpDir -> return (fst acc, snd acc + shiftAmount + roomOffset)
                        DownDir -> return (fst acc, snd acc - shiftAmount - roomOffset)
                        LeftDir -> return (fst acc - shiftAmount - roomOffset, snd acc)
                        RightDir -> return (fst acc + shiftAmount + roomOffset, snd acc)
                  else
                    return acc) (cx,cy)
              set p (grP { exits = filter (/= dir) (exits grP) })
              _ <- newEntity (Tile, Position (V2 (fx + px) (fy + py)), Sprite (loadSprite "tile.png") (tileSize, tileSize) Nothing)
              newEntity (roomTypeToGameRoom (rootLabel node) n (filter (/= oppositeDirection dir) exits'), Position (V2 (fx + px) (py + fy)))




          -- let (rw, rh) = getRoomSize (roomLayout (roomTypeToGameRoom (rootLabel node) n))
          -- -- For simplicity, place new rooms to the right of the parent room
          -- let newPos = Position (V2 (px + rw + tileSize) py)
          -- let gr = roomTypeToGameRoom (rootLabel node) n
          -- newEntity (gr, newPos)
  -- IDEA - implement a generic bfs which applies a function to each node
  -- make the function it applies a monadic function which checks for intersections across each GameRoom in the entity system currently
  -- and sets the position accordingly, and also inserts it into the map

connectionPosition :: Direction -> [[Char]] -> [[Char]] -> (Float, Float)
connectionPosition dir layout newLayout = case (directionCoord, oppDirectionCoord) of
    (Just (rA, cA), Just (rB, cB)) -> let
        wA = length (head layout)
        hA = length layout
        wB = length (head newLayout)
        hB = length newLayout
        (offXA, offYA) = tileToWorld wA hA cA rA tileSize
        (offXB, offYB) = tileToWorld wB hB cB rB tileSize
        cxB = offXA - offXB
        cyB = offYA - offYB
      in
        (cxB, cyB)
    _ -> error $ "No connection found for direction " ++ show dir ++ " in layouts: " ++ show layout ++ " and " ++ show newLayout
  where
    directionCoord = listToMaybe [ (r,c) | (r, row) <- zip [0..] layout, (c, ch) <- zip [0..] row, ch == intToDigit (fromEnum dir) ]
    oppDirectionCoord = listToMaybe [ (r,c) | (r, row) <- zip [0..] newLayout, (c, ch) <- zip [0..] row, ch == intToDigit (fromEnum (oppositeDirection dir)) ]
    tileToWorld w h tx ty size = let
        halfAdjust v = if even v then size / 2 else 0
        offsetX = (fromIntegral tx - fromIntegral (w - 1) / 2) * size + halfAdjust w
        offsetY = (fromIntegral (h - 1) / 2 - fromIntegral ty) * size + halfAdjust h
      in
        (offsetX, offsetY)
-- connectionPosition :: Direction -> [[Char]] -> [[Char]] -> (Float, Float)
-- connectionPosition dir layout newLayout = let
--     directionCoord = listToMaybe $ [ (row, col) | (row, line) <- zip [0..] layout, (col, c) <- zip [0..] line, c == head (show $ fromEnum dir) ]
--     oppDirectionCoord = listToMaybe $ [ (row, col) | (row, line) <- zip [0..] newLayout, (col, c) <- zip [0..] line, c == head (show $ fromEnum (oppositeDirection dir)) ]
--     (my,mx) = (fromIntegral (length layout - 1) / 2, fromIntegral (length (head layout) - 1) / 2)
--     (moy, mox) = (fromIntegral (length newLayout - 1) / 2, fromIntegral (length (head newLayout) - 1) / 2)
--     (newX, newY) = getRoomSize newLayout
--     (offsetX, offsetY) = case oppDirectionCoord of
--       Just (r,c) -> let 
--           (toX, toY) = (-((c - mox) * tileSize), -((moy - r) * tileSize))
--         in
--           case dir of
--             UpDir -> (0, roomOffset + newY + toY)
--             DownDir -> (0, -roomOffset - newY + toY)
--             LeftDir -> (-roomOffset - newX + toX, 0)
--             RightDir -> (roomOffset + newX + toX, 0)
--       Nothing -> error $ "No opposite connection found in specified direction " ++ show (oppositeDirection dir) ++ " for layout: " ++ show layout

--   in
--     case directionCoord of
--       Just (r,c) -> ((c - mx) * tileSize + offsetX, (r - my) * tileSize + offsetY)
--       Nothing -> let
--           errList = [ (col - mx, my - row, c) | (row, line) <- zip [0..] layout, (col, c) <- zip [0..] line, show c `elem` ["'1'", "'2'", "'3'", "'4'"] ]
--         in error $ "No connection found in specified direction " ++ show dir ++ " for layout: " ++ show layout ++ " found connections at: " ++ show errList ++ " (mx,my): " ++ show (mx,my) ++ " (length layout): " ++ show (length layout, length (head layout))

checkRoomIntersectionInDirection :: Position -> GameRoom -> Direction -> [[Char]] -> System' Bool
checkRoomIntersectionInDirection (Position (V2 x y)) gr dir newLayout = do
  let
    (cx, cy) = connectionPosition dir (roomLayout gr) newLayout
    (nx, ny) = (x + cx, y + cy)
  cfold (\acc (gr', Position (V2 xgr ygr)) ->
    acc || (let (rw', rh') = getRoomSize (roomLayout gr')
                intersectsX = abs (nx - xgr) < (rw'/2)
                intersectsY = abs (ny - ygr) < (rh'/2)
            in
              intersectsX && intersectsY)) False

roomTypeToGameRoom :: RoomType -> Int -> [Direction] -> GameRoom
roomTypeToGameRoom StartRoom _ exits' = GameRoom { roomType = StartRoom, roomLayout = head gameRoomLayouts, exits = exits' }
roomTypeToGameRoom rt n exits' = GameRoom { roomType = rt, roomLayout = getRoomLayout n, exits = exits' }

getRoomLayout :: Int -> [String]
getRoomLayout n = gameRoomLayouts !! (n `mod` length gameRoomLayouts)

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