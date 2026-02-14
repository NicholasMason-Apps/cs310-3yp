{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}

module Menu (stepMenu) where

import Apecs
import Types
import Utils (startTransition)
import Control.Monad (when)
import qualified Data.Set as Set
import Linear
import qualified Data.Map as Map
import Data.List (isInfixOf)

stepMenu :: Float -> System' ()
stepMenu dT = do
    stepButtons
    -- KeysPressed ks <- get global
    -- when (GkSpace `Set.member` ks) $ do
    --     startTransition (pi / 4) 1.0 StartDungeon

stepButtons :: System' ()
stepButtons = do
    MousePosition (V2 mx my) <- get global
    KeysPressed ks <- get global
    SpriteMap smap <- get global
    cmapM_ $ \(Button action, Position (V2 x y), SpriteRef sref m, e) -> do
        let
            Sprite (w,h) _ = smap Map.! sref
            baseSref = (if "-hover" `isInfixOf` sref then take (length sref - 6) sref else sref)
            posCheck = mx >= x - fromIntegral w / 2 && mx <= x + fromIntegral w / 2 &&
                       my >= y - fromIntegral h / 2 && my <= y + fromIntegral h / 2
        if posCheck then
            set e $ SpriteRef (baseSref ++ "-hover") m
        else
            set e $ SpriteRef baseSref m
        when (posCheck && GkLMB `Set.member` ks) $ do
            case action of
                StartGameButton -> startTransition (pi / 4) 1.0 StartDungeon
        