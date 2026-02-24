{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gloss.Draw ( draw, getSpritePicture ) where

import Apecs
import Apecs.Gloss
import Linear
import Types
import Codec.Picture
import Graphics.Gloss.Juicy (fromDynamicImage)
import Data.Maybe
import qualified Data.Vector as V
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.Random
import Utils
import System.IO.Unsafe ( unsafePerformIO )
import Systems
import Dungeon
import Graphics.Gloss.Interface.Environment (getScreenSize)

getSprite :: Map.Map String Sprite -> SpriteRef -> Sprite
getSprite smap (SpriteRef sr _) = smap Map.! sr

drawTransition :: System' Picture
drawTransition = foldDraw $ \(Transition p ang _ _ _) ->
    let t = easeInOut (min 1 p)
        dist = Utils.lerp (-2000) 2000 t
        dx = dist * cos ang
        dy = dist * sin ang
        w = 2500
        h = 2500
        rect = color black $ polygon [(-w/2,-h/2), (w/2,-h/2), (w/2,h/2), (-w/2,h/2)]
    in translate dx dy $ Apecs.Gloss.rotate (ang * 180/pi) rect

draw :: System' Picture
draw = do
    gs <- get global
    SpriteMap smap <- get global
    settings <- get global :: System' Settings
    Viewport (w, h) <- get global
    (w',h') <- liftIO getScreenSize
    drawTransitionPic <- drawTransition
    particles <- foldDraw $ \(Particle _, pos, s) -> translate' pos $ getSpritePicture smap s
    let
        scaleFactorX = if fullscreen settings then
            fromIntegral w' / 1280
        else
            fromIntegral w / 1280
        scaleFactorY = if fullscreen settings then
            fromIntegral h' / 720
        else
            fromIntegral h / 720
        scaleFactor = min scaleFactorX scaleFactorY
    p <- case gs of
        DungeonState -> drawDungeon
        CombatState  -> drawCombat
        MenuState -> drawMenu
        SettingsState -> drawSettings
        _ -> return $ color white $ scale 0.3 0.3 $ Text "Menu / Paused / Game Over Screen"
    floatingText <- foldDraw $ \(FloatingText _ _, pos, TextLabel str) -> translate' pos $ color white $ scale 0.1 0.1 $ Text str
    playerHealth <- foldDraw $ \(Player, Health hp) -> color white . translate (-620) 320 . scale 0.1 0.1 . Text $ "Health: " ++ show hp
    return $ scale scaleFactor scaleFactor (p <> particles <> floatingText <> playerHealth <> drawTransitionPic )

drawSettings :: System' Picture
drawSettings = do
    SpriteMap smap <- get global
    let settingsScreen = getSpritePicture smap (SpriteRef "settings-screen" Nothing)
    buttons <- foldDraw $ \(SettingsUIElement, Button _, pos, SpriteRef sref m) -> translate' pos $ getSpritePicture smap (SpriteRef sref m)
    return $ settingsScreen <> buttons

drawMenu :: System' Picture
drawMenu = do
    SpriteMap smap <- get global
    let titleScreen = getSpritePicture smap (SpriteRef "title-screen" Nothing)
    buttons <- foldDraw $ \(MainMenuUIElement, Button _, pos, SpriteRef sref m) -> translate' pos $ getSpritePicture smap (SpriteRef sref m)
    return $ titleScreen <> buttons

drawDungeon :: System' Picture
drawDungeon = do
    KeysPressed ks <- get global
    SpriteMap smap <- get global
    playerPos <- cfold (\_ (Player, p) -> Just p) Nothing
    player <- foldDraw $ \(Player, pos, s) -> let
            playerPic = getSpritePicture smap s
        in
            if GkLeft `Set.member` ks && GkRight `Set.notMember` ks then translate' pos $ scale (-1) 1 playerPic else translate' pos playerPic
    enemies <- foldDraw $ \(Enemy _, pos, s) -> translate' pos $ getSpritePicture smap s
    items <- foldDraw $ \(Item, pos, s) -> translate' pos $ getSpritePicture smap s
    tiles <- foldDraw $ \(Tile, pos, s) -> if isSpriteInView playerPos (getSprite smap s) pos
        then translate' pos $ getSpritePicture smap s
        else Blank
    let world = tiles <> player <> enemies <> items
    let camera = case playerPos of
            Just (Position (V2 x y)) -> translate (-x) (-y) world
            Nothing       -> world
    return camera

drawCombat :: System' Picture
drawCombat = do
    SpriteMap smap <- get global
    CombatTurn turn <- get global
    uiState <- get global :: System' UIState
    let ui = if turn == PlayerTurn then case uiState of
            CombatAttackSelectUI -> getSpritePicture smap (SpriteRef "combat-attack-select-ui" Nothing)
            CombatMagicSelectUI  -> getSpritePicture smap (SpriteRef "combat-magic-select-ui" Nothing)
        else getSpritePicture smap (SpriteRef "combat-parry-ui" Nothing)
    player <- foldDraw $ \(CombatPlayer, pos, s) -> translate' pos $ scale 2 2 $ getSpritePicture smap s
    enemy <- foldDraw $ \(CombatEnemy _, pos, s) -> translate' pos $ scale (-2) 2 $ getSpritePicture smap s
    enemyPlayerLayer <- if turn == PlayerTurn || turn == PlayerAttacking then
            return $ enemy <> player
        else
            return $ player <> enemy
    tiles <- foldDraw $ \(CombatTile, pos, s) -> translate' pos $ getSpritePicture smap s
    return $ tiles <> enemyPlayerLayer <> ui