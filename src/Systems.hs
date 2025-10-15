{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Systems where

import Apecs
import Apecs.Gloss
import System.Random
import System.Exit
import Linear
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)
import Types

playerSpeed, bulletSpeed, enemySpeed, xmin, xmax :: Float
playerSpeed = 170
bulletSpeed = 500
enemySpeed  = 80
xmin = -640
xmax = 640

hitBonus, missPenalty :: Int
hitBonus = 100
missPenalty = 40

playerPos, scorePos :: V2 Float
playerPos = V2 0 0
scorePos  = V2 xmin (-170)

-- Initialise the game state by creating a player entity
initialize :: System' ()
initialize = do
    playerEntity <- newEntity (Player, Position playerPos, Velocity (V2 0 0), StaticSprite (color white $ rectangleSolid 10 10) (10,10) )
    wallEntity <- newEntity (Wall, Position (V2 150 150), StaticSprite (color blue $ rectangleSolid 50 50) (50,50) )
    return ()

stepPositionFormula :: Float -> Position -> Velocity -> Position
stepPositionFormula dT (Position p) (Velocity v) = Position (p + dT *^ v)

-- Update positions based on velocity and delta time
stepPosition :: Float -> System' ()
stepPosition dT = cmap $ uncurry (stepPositionFormula dT)

-- Lock the player's position within the screen bounds
clampPlayer :: System' ()
clampPlayer = cmap $ \(Player, Position (V2 x y)) -> Position (V2 (min xmax . max xmin $ x) y)

incrementTime :: Float -> System' ()
incrementTime dT = modify global $ \(Time t) -> Time (t + dT)

clearTargets :: System' ()
clearTargets = cmap $ \all@(Target, Position (V2 x _), Velocity _) ->
    if x < xmin || x > xmax
        then Nothing
        else Just all

-- Remove all particles that have expired
stepParticles :: Float -> System' ()
stepParticles dT = cmap $ \(Particle t) ->
    if t < 0
        then Right $ Not @(Particle, Kinetic) -- Not is used to delete the particle
        else Left $ Particle (t - dT)

-- Remove bullets that have gone off the top of the screen, penalizing the player
clearBullets :: System' ()
clearBullets = cmap $ \(Bullet, Position (V2 x y), Score s) ->
    if y > 170
        then Right $ (Not @(Bullet, Kinetic), Score (s - missPenalty)) -- Right of (Not (Bullet, Kinetic), Score) removes the bullet and decrements score
        else Left () -- Left used to do nothing

handleCollisions :: System' ()
handleCollisions = cmapM_ $ \(Target, Position posT, entityT) ->
    cmapM_ $ \(Bullet, Position posB, entityB) ->
        when (norm (posT - posB) < 10) $ do
            -- Remove the target
            destroy entityT (Proxy @(Target, Kinetic))
            -- Remove the bullet
            destroy entityB (Proxy @(Bullet, Kinetic))
            -- Spawn particles
            spawnParticles 15 (Position posB) (-500,500) (200,-50)
            modify global $ \(Score s) -> Score (s + hitBonus)

handlePlayerCollisions :: System' ()
handlePlayerCollisions = cmapM $ \(Player, posP, v, sp) ->
    cmapM $ \(Wall, Position posW, sw) -> do
        Time t <- get global
        let (Position posP') = stepPositionFormula t posP v -- Predict next position
        case checkBoundaryBoxIntersection posP' sp posW sw of
            Just dir -> case dir of
                North -> cmap $ \(Player, Velocity (V2 vx vy)) -> Velocity (V2 vx (vy + playerSpeed))
                South -> cmap $ \(Player, Velocity (V2 vx vy)) -> Velocity (V2 vx (vy - playerSpeed))
                East  -> cmap $ \(Player, Velocity (V2 vx vy)) -> Velocity (V2 (vx + playerSpeed) vy)
                West  -> cmap $ \(Player, Velocity (V2 vx vy)) -> Velocity (V2 (vx - playerSpeed) vy)
            Nothing -> return ()

-- handlePlayerCollisions :: System' ()
-- handlePlayerCollisions = cmapM $ \(Player, posP, v, s1) ->
--     cmapM $ \(Wall, Position posW, s2) -> let
--             (Position posP') = stepPositionFormula dT posP v -- Predict next position
--         in
--             case checkBoundaryBoxIntersection posP' s1 posW s2 of
--                 Just dir -> case dir of
--                     North -> cmap $ \(Player, Velocity (V2 vx vy)) -> Velocity (V2 vx (vy + playerSpeed))
--                     South -> cmap $ \(Player, Velocity (V2 vx vy)) -> Velocity (V2 vx (vy - playerSpeed))
--                     East  -> cmap $ \(Player, Velocity (V2 vx vy)) -> Velocity (V2 (vx + playerSpeed) vy)
--                     West  -> cmap $ \(Player, Velocity (V2 vx vy)) -> Velocity (V2 (vx - playerSpeed) vy)
--                 Nothing -> return ()

-- Boundary box collision detection
-- The Direction indicates which side the first sprite hit the second sprite from
-- Note: Sprite positions are centered based on their Position component
checkBoundaryBoxIntersection :: V2 Float -> Sprite -> V2 Float -> Sprite -> Maybe Direction
checkBoundaryBoxIntersection (V2 x1 y1) s1 (V2 x2 y2) s2
    | right1 > left2 && left1 < left2 && bottom1 > top2 && top1 < bottom2 = Just West
    | left1 < right2 && right1 > right2 && bottom1 > top2 && top1 < bottom2 = Just East
    | bottom1 > top2 && top1 < top2 && right1 > left2 && left1 < right2 = Just South
    | top1 < bottom2 && bottom1 > bottom2 && right1 > left2 && left1 < right2 = Just North
    | otherwise = Nothing
    where
        (w1,h1) = case s1 of
            StaticSprite _ (w,h) -> (toEnum w,toEnum h)
            SpriteSheet _ (w,h) n -> (toEnum $ w `div` n, toEnum h)
        (w2,h2) = case s2 of
            StaticSprite _ (w,h) -> (toEnum w, toEnum h)
            SpriteSheet _ (w,h) n -> (toEnum $ w `div` n, toEnum h)
        left1 = x1 - w1/2
        right1 = x1 + w1/2
        top1  = y1 - h1/2
        bottom1 = y1 + h1/2
        left2 = x2 - w2/2
        right2 = x2 + w2/2
        top2  = y2 - h2/2
        bottom2 = y2 + h2/2

triggerEvery :: Float -> Float -> Float -> System' a -> System' ()
triggerEvery dT period phase sys = do
    Time t <- get global
    let t' = t + phase
        trigger = floor (t'/period) /= floor ((t'+dT)/period)
    when trigger $ void sys

-- Spawn n particles at a given position with random velocities in the given ranges
-- dvx and dvy are (min,max) pairs for x and y velocity components
spawnParticles :: Int -> Position -> (Float,Float) -> (Float,Float) -> System' ()
spawnParticles n pos dvx dvy = replicateM_ n $ do
    vx <- liftIO $ randomRIO dvx
    vy <- liftIO $ randomRIO dvy
    t <- liftIO $ randomRIO (0.02, 0.3)
    newEntity (Particle t, pos, Velocity (V2 vx vy))

-- TODO: Add boundary box collision check and stop player movement
step :: Float -> System' ()
step dT = do
    incrementTime dT
    handlePlayerCollisions
    stepPosition dT
    clampPlayer
    clearTargets
    clearBullets
    stepParticles dT
    handleCollisions
    triggerEvery dT 0.6 0   $ newEntity (Target, Position (V2 xmin 80), Velocity (V2 enemySpeed 0))
    triggerEvery dT 0.6 0.3 $ newEntity (Target, Position (V2 xmax 120), Velocity (V2 (negate enemySpeed) 0))

handleEvent :: Event -> System' ()
-- Player movement with collision detection
-- handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) = cmapM $ \(Wall, Position posW, sw) -> 
--     cmapM $ \(Player, posP, Velocity (V2 x y), sp) -> do
--         Time t <- get global
--         let (Position posP') = stepPositionFormula t posP (Velocity (V2 (x - playerSpeed) y)) -- Predict next position
--         case checkBoundaryBoxIntersection posP' sp posW sw of
--             Just East -> return $ Velocity (V2 x y) -- Collision, do not move
--             _         -> return $ Velocity (V2 (x - playerSpeed) y) -- No collision
-- handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) = cmapM $ \(Wall, Position posW, sw) -> 
--     cmapM $ \(Player, posP, Velocity (V2 x y), sp) -> do
--         Time t <- get global
--         let (Position posP') = stepPositionFormula t posP (Velocity (V2 (x + playerSpeed) y)) -- Predict next position
--         case checkBoundaryBoxIntersection posP' sp posW sw of
--             Just East -> return $ Velocity (V2 x y) -- Collision, do not move
--             _         -> return $ Velocity (V2 (x + playerSpeed) y) -- No collision


-- Player movement
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) = cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 (x-playerSpeed) y)
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _)   = cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 (x+playerSpeed) y)
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) = cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 (x+playerSpeed) y)
handleEvent (EventKey (SpecialKey KeyRight) Up _ _)   = cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 (x-playerSpeed) y)
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) = cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 x (y+playerSpeed))
handleEvent (EventKey (SpecialKey KeyUp) Up _ _)   = cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 x (y-playerSpeed))
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) = cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 x (y-playerSpeed))
handleEvent (EventKey (SpecialKey KeyDown) Up _ _)   = cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 x (y+playerSpeed))
-- Player shooting
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) = cmapM_ $ \(Player, pos) -> do
    newEntity (Bullet, pos, Velocity (V2 0 bulletSpeed))
    spawnParticles 7 pos (-80,80) (10,100)
-- Exit game
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) = liftIO exitSuccess
handleEvent _ = return () -- base case

translate' :: Position -> Picture -> Picture
translate' (Position (V2 x y)) = translate x y

triangle, diamond :: Picture
triangle = Line [(0,0),(-0.5,-1),(0.5,-1),(0,0)]
diamond  = Line [(-1,0),(0,-1),(1,0),(0,1),(-1,0)]

-- renderSprite :: Sprite -> Position -> Picture
-- renderSprite (Sprite path (ox,oy) (w,h)) pos = translate' pos $ color white $ 

draw :: System' Picture
draw = do
    player <- foldDraw $ \(Player, pos, StaticSprite s _) -> translate' pos s -- TODO: adapt types to not have non-exhaustive pattern matching
    targets <- foldDraw $ \(Target, pos) -> translate' pos $ color red $ scale 10 10 diamond
    bullets <- foldDraw $ \(Bullet, pos) -> translate' pos $ color yellow $ scale 4 4 $ diamond
    particles <- foldDraw $ \(Particle _, Velocity (V2 vx vy), pos) ->
        translate' pos $ color orange $ Line [(0,0),(vx/10, vy/10)]
    wall <- foldDraw $ \(Wall, pos, StaticSprite s _) -> translate' pos s
    Score s <- get global
    let score = color white $ translate' (Position scorePos) $ scale 0.1 0.1 $ Text $ "Score: " ++ show s
    playerPos <- cfold (\_ (Player, Position p) -> Just p) Nothing
    let playerPosText = case playerPos of
            Just (V2 x y) -> color white $ translate' (Position (V2 (x-50) (y+20))) $ scale 0.1 0.1 $ Text $ "(" ++ show (round x) ++ "," ++ show (round y) ++ ")"
            Nothing       -> Blank
    -- let dot = color blue $ translate' (Position (V2 0 0)) $ rectangleSolid 1 1
    collisionRes <- cfoldM (\_ (Player, Position p1, s1) ->
            cfold (\_ (Wall, Position p2, s2) -> checkBoundaryBoxIntersection p1 s1 p2 s2) Nothing) Nothing
    let collisionText = case collisionRes of
            Just dir -> color white $ translate 10 10 $ scale 0.1 0.1 $ Text $ "Collision: " ++ show dir
            Nothing  -> color white $ translate 10 10 $ scale 0.1 0.1 $ Text "Collision: None"
    let world = player <> targets <> bullets <> score <> particles <> wall <> playerPosText <> collisionText
    -- let camera = case playerPos of
    --         Just (V2 x y) -> translate (-x) (-y) world
    --         Nothing       -> world
    return world

