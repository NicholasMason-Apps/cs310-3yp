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
import Control.Monad
import Data.Maybe ( isJust, fromMaybe )
import qualified Data.Set as Set

playerAttackFrames :: Set.Set Int
playerAttackFrames = Set.fromList [7]

enemySkeletonAttackFrames :: Set.Set Int
enemySkeletonAttackFrames = Set.fromList [7]

enemyVampireAttackFrames :: Set.Set Int
enemyVampireAttackFrames = Set.fromList [11]

enemyReaperAttackFrames :: Set.Set Int
enemyReaperAttackFrames = Set.fromList [6, 11]

playerDamage :: Int
playerDamage = 10

stepPlayerTurn :: Float -> System' ()
stepPlayerTurn dT = do
    KeysPressed ks <- get global
    when (SpecialKey KeySpace `Set.member` ks) $ do
        set global $ CombatTurn PlayerAttacking
        cmapM_ $ \(CombatPlayer, s) -> do
            set s (SpriteRef "player-knife-attack" (Just 0))
            set s (Position (V2 ((1280 / 3) - tileSize) 0))

stepPlayerAttack :: Float -> System' ()
stepPlayerAttack dT = do
    cmapM_ $ \(CombatPlayer, SpriteRef sr n, e) -> do
        when (sr == "player-idle") $ do
            set global $ CombatTurn EnemyTurn
            set e (Position (V2 ((-1280 / 3)) 0))
        when (fromMaybe 0 n `Set.member` playerAttackFrames) $ cmapM_ $ \(CombatEnemy e', SpriteRef sr' _, ce) -> do
            enemy <- get e' :: System' Enemy
            case enemyType enemy of
                Reaper -> when (sr' /= "reaper-hit") $ do
                    modify e' $ \(Health hp) -> Health (hp - playerDamage)
                    set ce (SpriteRef "reaper-hit" (Just 1))
                Vampire -> when (sr' /= "vampire-hit") $ do
                    modify e' $ \(Health hp) -> Health (hp - playerDamage)
                    set ce (SpriteRef "vampire-hit" (Just 1))
                Skeleton -> when (sr' /= "skeleton-hit") $ do
                    modify e' $ \(Health hp) -> Health (hp - playerDamage)
                    set ce (SpriteRef "skeleton-hit" (Just 1))

stepEnemyAttack :: Float -> System' ()
stepEnemyAttack dT = do
    cmapM_ $ \(CombatEnemy e', SpriteRef sr n, e) -> do
        when (sr == "skeleton-idle" || sr == "vampire-idle" || sr == "reaper-idle") $ do
            set global $ CombatTurn PlayerTurn
            set e (Position (V2 (1280 / 3) 0))
        enemy <- get e' :: System' Enemy
        case enemyType enemy of
            Skeleton -> when (fromMaybe 0 n `Set.member` enemySkeletonAttackFrames) $ cmapM_ $ \(CombatPlayer, cp, SpriteRef sr' _) -> do
                when (sr' /= "player-hit") $ do
                    cmap $ \(Player, Health hp) -> Health (hp - playerDamage)
                    set cp (SpriteRef "player-hit" (Just 1))
            Vampire -> when (fromMaybe 0 n `Set.member` enemyVampireAttackFrames) $ cmapM_ $ \(CombatPlayer, cp, SpriteRef sr' _) -> do
                when (sr' /= "player-hit") $ do
                    cmap $ \(Player, Health hp) -> Health (hp - playerDamage)
                    set cp (SpriteRef "player-hit" (Just 1))
            Reaper -> when (fromMaybe 0 n `Set.member` enemyReaperAttackFrames) $ cmapM_ $ \(CombatPlayer, cp, SpriteRef sr' _) -> do
                when (sr' /= "player-hit") $ do
                    cmap $ \(Player, Health hp) -> Health (hp - playerDamage)
                    set cp (SpriteRef "player-hit" (Just 1))
stepEnemyTurn :: Float -> System' ()
stepEnemyTurn dT = do
    cmapM_ $ \(CombatEnemy _, SpriteRef sr _, e) -> do
        case sr of
            "skeleton-idle" -> do
                set e (SpriteRef "skeleton-attack" (Just 0))
                set e (Position (V2 ((-1280 / 3) + 64) 0))
                set global $ CombatTurn EnemyAttacking
            "vampire-idle"  -> do
                set e (SpriteRef "vampire-attack" (Just 0))
                set e (Position (V2 ((-1280 / 3) + 64) 0))
                set global $ CombatTurn EnemyAttacking
            "reaper-idle"   -> do
                set e (SpriteRef "reaper-attack" (Just 0))
                set e (Position (V2 ((-1280 / 3) + 64) 0))
                set global $ CombatTurn EnemyAttacking
            _               -> return ()

stepCombat :: Float -> System' ()
stepCombat dT = do
    ce <- cfold (\_ (CombatEnemy ce) -> Just ce) Nothing
    CombatTurn turn <- get global
    case ce of
        Nothing -> return ()
        Just e -> case turn of
            PlayerTurn -> stepPlayerTurn dT
            EnemyTurn -> stepEnemyTurn dT
            PlayerAttacking -> stepPlayerAttack dT
            EnemyAttacking -> stepEnemyAttack dT


drawCombat :: System' Picture
drawCombat = do
    SpriteMap smap <- get global
    CombatTurn turn <- get global
    let ui = if turn == PlayerTurn then getSpritePicture smap (SpriteRef "combat-ui" Nothing) else Blank
    player <- foldDraw $ \(CombatPlayer, pos, s) -> translate' pos $ scale 2 2 $ getSpritePicture smap s
    enemy <- foldDraw $ \(CombatEnemy _, pos, s) -> translate' pos $ scale (-2) 2 $ getSpritePicture smap s
    enemyPlayerLayer <- if turn == PlayerTurn || turn == PlayerAttacking then
            return $ enemy <> player
        else
            return $ player <> enemy
    tiles <- foldDraw $ \(CombatTile, pos, s) -> translate' pos $ getSpritePicture smap s
    return $ tiles <> enemyPlayerLayer <> ui