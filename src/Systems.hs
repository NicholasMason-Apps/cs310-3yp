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

-- Block the player from moving into walls
blockPlayer :: System' ()
blockPlayer = cmapM $ \(Wall, Position posW, spriteW) ->
    cmapM $ \(Player, Position posP, spriteP) -> do
        let top = checkBoundaryBoxTopIntersection posP spriteP posW spriteW
            bottom = checkBoundaryBoxBottomIntersection posP spriteP posW spriteW
            left = checkBoundaryBoxLeftIntersection posP spriteP posW spriteW
            right = checkBoundaryBoxRightIntersection posP spriteP posW spriteW
            (wp, hp) = case spriteP of
                StaticSprite _ (w,h) -> (toEnum w,toEnum h)
                SpriteSheet _ (w,h) n -> (toEnum $ w `div` n, toEnum h)
            (ww, hw) = case spriteW of
                StaticSprite _ (w,h) -> (toEnum w,toEnum h)
                SpriteSheet _ (w,h) n -> (toEnum $ w `div` n, toEnum h)
            (V2 x _) = posP
            (V2 xw yw) = posW
        posP' <- if top then return $ Position (V2 x (yw + (hw / 2) + (hp / 2))) else return $ Position posP
        let (Position (V2 x' _)) = posP'
        posP'' <- if bottom then return $ Position (V2 x' (yw - (hw / 2) - (hp / 2))) else return posP'
        let (Position (V2 _ y'')) = posP''
        posP''' <- if left then return $ Position (V2 (xw - (ww / 2) - (wp / 2)) y'') else return posP''
        let (Position (V2 _ y''')) = posP'''
        return $ if right then Position (V2 (xw + (ww / 2) + (wp / 2)) y''') else posP'''

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

-- Boundary box collision detection
-- Note: Sprite positions are centered based on their Position component
checkBoundaryBoxTopIntersection :: V2 Float -> Sprite -> V2 Float -> Sprite -> Bool
checkBoundaryBoxTopIntersection (V2 x1 y1) s1 (V2 x2 y2) s2 =
    top1 < bottom2 && bottom1 > bottom2 && right1 > left2 && left1 < right2
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
        bottom2 = y2 + h2/2
checkBoundaryBoxBottomIntersection :: V2 Float -> Sprite -> V2 Float -> Sprite -> Bool
checkBoundaryBoxBottomIntersection (V2 x1 y1) s1 (V2 x2 y2) s2 =
    bottom1 > top2 && top1 < top2 && right1 > left2 && left1 < right2
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
checkBoundaryBoxLeftIntersection :: V2 Float -> Sprite -> V2 Float -> Sprite -> Bool
checkBoundaryBoxLeftIntersection (V2 x1 y1) s1 (V2 x2 y2) s2 =
    right1 > left2 && left1 < left2 && bottom1 > top2 && top1 < bottom2
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
        top2  = y2 - h2/2
        bottom2 = y2 + h2/2
checkBoundaryBoxRightIntersection :: V2 Float -> Sprite -> V2 Float -> Sprite -> Bool
checkBoundaryBoxRightIntersection (V2 x1 y1) s1 (V2 x2 y2) s2 =
    left1 < right2 && right1 > right2 && bottom1 > top2 && top1 < bottom2
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
    -- handlePlayerCollisions
    stepPosition dT
    clampPlayer
    blockPlayer
    clearTargets
    clearBullets
    stepParticles dT
    handleCollisions
    triggerEvery dT 0.6 0   $ newEntity (Target, Position (V2 xmin 80), Velocity (V2 enemySpeed 0))
    triggerEvery dT 0.6 0.3 $ newEntity (Target, Position (V2 xmax 120), Velocity (V2 (negate enemySpeed) 0))

handleEvent :: Event -> System' ()
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

animateSprites :: System' ()
animateSprites = cmapM $ \(SpriteSheet pic (w,h) n) -> do
    Time t <- get global
    FPS fps <- get global
    -- TODO: add animation rendering by figuring out how to do a rendering rectangle
    -- also need to add a frame time component to control animation speed

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
    let world = player <> targets <> bullets <> score <> particles <> wall <> playerPosText
    let camera = case playerPos of
            Just (V2 x y) -> translate (-x) (-y) world
            Nothing       -> world
    return camera

