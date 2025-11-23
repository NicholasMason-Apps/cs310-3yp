{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Combat where

import Apecs
import Apecs.Gloss
import Linear
import Types
import Graphics.Gloss
import Utils
import Sprite

stepCombat :: Float -> System' ()
stepCombat dT = do
    return ()

drawCombat :: System' Picture
drawCombat = do
    SpriteMap smap <- get global
    CombatEnemy e <- get global
    let ui = getSpritePicture smap (SpriteRef "combat-ui" Nothing)
    player <- foldDraw $ \(Player, s) -> translate' (Position (V2 (-1280 / 3) 0)) $ scale 2 2 $ getSpritePicture smap s
    enemy <- case e of
        Nothing -> return Blank
        Just ent -> get ent >>= \(Enemy _, s) -> return $ translate' (Position (V2 (1280 / 3) 0)) $ scale (-2) 2 $ getSpritePicture smap s
    tiles <- foldDraw $ \(CombatTile, pos, s) -> translate' pos $ getSpritePicture smap s
    return $ tiles <> enemy <> player <> ui