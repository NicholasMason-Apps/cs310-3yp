{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeApplications           #-}

module Settings (stepSettings) where

import Apecs
import Types
import qualified Data.Set as Set
import Linear
import qualified Data.Map as Map
import qualified Data.Vector as V
import Menu (buttonActions, posCheck)
import Control.Monad (when)
import Data.List (isInfixOf)
import Utils (startTransition)

stepSettings :: Float -> System' ()
stepSettings dT = do
    stepButtons
    KeysPressed ks <- get global
    when (GkEsc `Set.member` ks) $ do
        startTransition (pi / 4) 1.0 ToMenu

stepButtons :: System' ()
stepButtons = do
    MousePosition (V2 mx my) <- get global
    KeysPressed ks <- get global
    SpriteMap smap <- get global
    cmapM_ $ \(SettingsUIElement, ButtonGroup group active, e) -> do
        V.mapM_ (\e' -> do
            Button action <- get e'
            Position (V2 x y) <- get e'
            SpriteRef sref m <- get e'
            let
                Sprite (w,h) _ = smap Map.! sref
                baseSref = (if "-hover" `isInfixOf` sref then take (length sref - 6) sref else sref)
            if posCheck mx my x y w h then
                set e' $ SpriteRef (baseSref ++ "-hover") m
            else
                set e' $ SpriteRef baseSref m
            when (posCheck mx my x y w h && GkLMB `Set.member` ks) $ do
                buttonActions action
                set e $ ButtonGroup group e'
            ) group
        SpriteRef srefA mA <- get active
        let baseSrefA = (if "-hover" `isInfixOf` srefA then take (length srefA - 6) srefA else srefA)
        set active $ SpriteRef (baseSrefA ++ "-hover") mA
        
