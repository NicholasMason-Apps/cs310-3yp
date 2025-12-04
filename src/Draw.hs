{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Draw ( draw ) where

import Apecs
import Linear
import Types
import Data.Maybe
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Random
import Utils
import System.IO.Unsafe ( unsafePerformIO )
import Combat
import Dungeon
import qualified SDL

drawTransition :: System' ()
drawTransition = foldDraw $ \(Transition p ang _ _) -> 
    let t = easeInOut (min 1 p)
        dist = Utils.lerp (-2000) 2000 t
        dx = dist * cos ang
        dy = dist * sin ang
        w = 2500
        h = 2500
        rect = color black $ polygon [(-w/2,-h/2), (w/2,-h/2), (w/2,h/2), (-w/2,h/2)]
    in translate dx dy $ Apecs.Gloss.rotate (ang * 180/pi) rect

draw :: SDL.Renderer -> FPS -> System' ()
draw r fps = do
    gs <- get global
    case gs of
        DungeonState -> drawDungeon r fps
        CombatState  -> drawCombat r fps
        _ -> drawMenuOrPause r fps
    -- Viewport (w, h) <- get global
    -- drawTransitionPic <- drawTransition
    -- playerHealth <- foldDraw $ \(Player, Health hp) -> color white . translate (-600) 300 . scale 0.1 0.1 . Text $ "Health: " ++ show hp
    -- let
    --     scaleFactorX = fromIntegral w / 1280
    --     scaleFactorY = fromIntegral h / 720
    --     scaleFactor = min scaleFactorX scaleFactorY
    -- p <- case gs of
    --     DungeonState -> drawDungeon
    --     CombatState  -> drawCombat
    --     _ -> return $ color white $ scale 0.3 0.3 $ Text "Menu / Paused / Game Over Screen"
    -- return $ scale scaleFactor scaleFactor (p <> drawTransitionPic <> playerHealth)