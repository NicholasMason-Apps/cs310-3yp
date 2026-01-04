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
import Data.Maybe ( isJust, fromMaybe, isNothing )
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified SDL

playerKnifeAttackFrames :: Set.Set Int
playerKnifeAttackFrames = Set.fromList [7]

playerMagicAttackFrames :: Set.Set Int
playerMagicAttackFrames = Set.fromList [6]

enemySkeletonAttackFrames :: Set.Set Int
enemySkeletonAttackFrames = Set.fromList [7]

enemyVampireAttackFrames :: Set.Set Int
enemyVampireAttackFrames = Set.fromList [11]

enemyReaperAttackFrames :: Set.Set Int
enemyReaperAttackFrames = Set.fromList [6, 11]

enemyGoldenReaperAttackFrames :: Set.Set Int
enemyGoldenReaperAttackFrames = Set.fromList [6, 11]

playerDamage :: Int
playerDamage = 20

enemyDamage :: Int
enemyDamage = 5

stepPlayerTurn :: Float -> System' ()
stepPlayerTurn dT = do
    KeysPressed ks <- get global
    uiState <- get global :: System' UIState
    case uiState of
        CombatAttackSelectUI -> do
            when (GkSpace `Set.member` ks) $ do
                set global $ KeysPressed (GkSpace `Set.delete` ks)
                set global $ CombatTurn PlayerAttacking
                cmapM_ $ \(CombatPlayer, s) -> do
                    set s (SpriteRef "player-knife-attack" (Just 0)) 
                    set s (Position (V2 ((1280/3) - tileSize) 0))
            when (GkE `Set.member` ks) $ do
                set global CombatMagicSelectUI
                set global $ KeysPressed (GkE `Set.delete` ks)
        CombatMagicSelectUI -> do
            when (GkE `Set.member` ks) $ do
                set global $ CombatTurn PlayerAttacking
                set global CombatAttackSelectUI
                set global $ KeysPressed $ (GkE `Set.delete` ks)
                cmapM_ $ \(CombatPlayer, s) -> set s (SpriteRef "player-fire-attack" (Just 0))
            when (GkQ `Set.member` ks) $ do
                set global $ CombatTurn PlayerAttacking
                set global CombatAttackSelectUI
                set global $ KeysPressed $ (GkQ `Set.delete` ks)
                cmapM_ $ \(CombatPlayer, s) -> set s (SpriteRef "player-prismatic-attack" (Just 0))
            when (GkEsc `Set.member` ks) $ do
                set global CombatAttackSelectUI
                set global $ KeysPressed $ (GkEsc `Set.delete` ks)

stepPlayerAttack :: Float -> System' ()
stepPlayerAttack dT = do
    cmapM_ $ \(CombatPlayer, SpriteRef sr n, e) -> do
        particle <- cfold (\_ (CombatAttackParticle e) -> Just e) Nothing
        when (isNothing particle && (sr == "player-fire-attack" || sr == "player-prismatic-attack") && (fromMaybe 0 n `Set.member` playerMagicAttackFrames)) $ do
            if sr == "player-fire-attack" then do
                particle <- spawnParticle (Position (V2 ((-1280 / 3) + tileSize / 2) 16)) (Position (V2 (1280 / 3) 0)) "particle-fire" 11
                void $ newEntity (CombatAttackParticle particle)
            else when (sr == "player-prismatic-attack") $ do
                particle <- spawnParticle (Position (V2 ((-1280 / 3) + tileSize / 2) 16)) (Position (V2 (1280 / 3) 0)) "particle-prismatic" 13
                void $ newEntity (CombatAttackParticle particle)
        Particle (Position destPos) <- case particle of
            Just p -> get p :: System' Particle
            Nothing -> return $ Particle (Position (V2 0 0))
        Position currPos <- case particle of
            Just p -> do
                Position pos <- get p :: System' Position
                return $ Position pos
            Nothing -> return $ Position (V2 100 100)
        let attackHitCondition = (fromMaybe 0 n `Set.member` playerKnifeAttackFrames && sr == "player-knife-attack")
                                || (norm (destPos - currPos) < 20)
        when attackHitCondition $ cmapM_ $ \(CombatEnemy e', SpriteRef sr' _, ce) -> do
            enemy <- get e' :: System' Enemy
            Health hp <- get e' :: System' Health
            if hp - playerDamage > 0 then
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
                    GoldenReaper -> when (sr' /= "golden-reaper-hit") $ do
                        modify e' $ \(Health hp) -> Health (hp - playerDamage)
                        set ce (SpriteRef "golden-reaper-hit" (Just 1))
            else
                case enemyType enemy of
                    Reaper -> when (sr' /= "reaper-death") $ do
                        set ce (SpriteRef "reaper-death" (Just 1))
                        set global $ CombatTurn PlayerWin
                    Vampire -> when (sr' /= "vampire-death") $ do
                        set ce (SpriteRef "vampire-death" (Just 1))
                        set global $ CombatTurn PlayerWin
                    Skeleton -> when (sr' /= "skeleton-death") $ do
                        set ce (SpriteRef "skeleton-death" (Just 1))
                        set global $ CombatTurn PlayerWin
                    GoldenReaper -> when (sr' /= "golden-reaper-death") $ do
                        set ce (SpriteRef "golden-reaper-death" (Just 1))
                        set global $ CombatTurn PlayerWin
        when (sr == "player-idle" && isNothing particle) $ do
            set global $ CombatTurn EnemyTurn
            set e (Position (V2 ((-1280 / 3)) 0))

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
                    cmap $ \(Player, Health hp) -> Health (hp - enemyDamage)
                    set cp (SpriteRef "player-hit" (Just 1))
            Vampire -> when (fromMaybe 0 n `Set.member` enemyVampireAttackFrames) $ cmapM_ $ \(CombatPlayer, cp, SpriteRef sr' _) -> do
                when (sr' /= "player-hit") $ do
                    cmap $ \(Player, Health hp) -> Health (hp - enemyDamage)
                    set cp (SpriteRef "player-hit" (Just 1))
            Reaper -> when (fromMaybe 0 n `Set.member` enemyReaperAttackFrames) $ cmapM_ $ \(CombatPlayer, cp, SpriteRef sr' _) -> do
                when (sr' /= "player-hit") $ do
                    cmap $ \(Player, Health hp) -> Health (hp - enemyDamage)
                    set cp (SpriteRef "player-hit" (Just 1))
            GoldenReaper -> when (fromMaybe 0 n `Set.member` enemyGoldenReaperAttackFrames) $ cmapM_ $ \(CombatPlayer, cp, SpriteRef sr' _) -> do
                when (sr' /= "player-hit") $ do
                    cmap $ \(Player, Health hp) -> Health (hp - enemyDamage)
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
            "golden-reaper-idle"   -> do
                set e (SpriteRef "golden-reaper-attack" (Just 0))
                set e (Position (V2 ((-1280 / 3) + 64) 0))
                set global $ CombatTurn EnemyAttacking
            _               -> return ()

stepPlayerWin :: Float -> System' ()
stepPlayerWin dT = cmapM_ $ \(CombatEnemy _, SpriteRef sr n) -> do
    cmapIf (\(CombatPlayer, SpriteRef sr' _) -> sr' == "player-idle") (\CombatPlayer -> Position (V2 ((-1280 / 3)) 0))
    when (sr == "vampire-death" || sr == "skeleton-death" || sr == "reaper-death") $ do
        SpriteMap smap <- get global
        let Sprite _ spriteE = smap Map.! sr
            anim = case spriteE of
                GlossRenderer (Right a) -> a
                GlossRenderer (Left _) -> error "Static sprite does not support frame number"
                SDLRenderer (_, Nothing) -> error "Static sprite does not support frame number"
                SDLRenderer (_, Just a) -> a
        existsTransition <- cfold (\_ (Transition {}) -> Just ()) Nothing
        when (fromMaybe 0 n + 1 >= frameCount anim && isNothing existsTransition) $ startTransition (pi / 4) 1.0 ToDungeon

stepCombat :: Float -> System' ()
stepCombat dT = do
    ce <- cfold (\_ (CombatEnemy ce) -> Just ce) Nothing
    CombatTurn turn <- get global
    playerHealth <- cfold (\_ (Player, Health hp) -> Just hp) Nothing
    when (isJust playerHealth && fromMaybe 0 playerHealth <= 0) $ liftIO $ putStrLn "Player has been defeated!"
    stepPosition dT
    case ce of
        Nothing -> return ()
        Just e -> case turn of
            PlayerTurn -> stepPlayerTurn dT
            EnemyTurn -> stepEnemyTurn dT
            PlayerAttacking -> stepPlayerAttack dT
            EnemyAttacking -> stepEnemyAttack dT
            PlayerWin -> stepPlayerWin dT