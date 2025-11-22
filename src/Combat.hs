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
    player <- foldDraw $ \(Player, s) -> translate' (Position (V2 (-1280 / 3) 0)) $ getSpritePicture smap s
    enemy <- get e >>= \(Enemy _, s) -> return $ translate' (Position (V2 (1280 / 3) 0)) $ scale (-1) 1 $ getSpritePicture smap s
    tiles <- foldDraw $ \(CombatTile, pos, s) -> translate' pos $ getSpritePicture smap s
    return $ tiles <> enemy <> player