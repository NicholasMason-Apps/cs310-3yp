module Main (main) where

import Types
import Systems
import Apecs
import Apecs.Gloss
import Draw
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef

main :: IO ()
main = do
    w <- initWorld
    runWith w $ do
        initialize
        play (InWindow "Shmup" (1280, 720) (10, 10)) (makeColorI 37 19 26 255) 60 draw handleEvent step

initSprites :: IO ()
initSprites = do
    let spriteList = [ (
                            "player",
                            Sprite (32,32) (Right $ Animations {
                                idle = Animation { frameCount = 9, currentFrame = 1, frameSpeed = 0.3, sprites = loadAnimatedSprite "player/player-idle.png" 9 (288,32) },
                                walk = Animation { frameCount = 11, currentFrame = 1, frameSpeed = 0.1, sprites = loadAnimatedSprite "player/player-walk.png" 11 (352,32) }
                            }) 
                        ),
                        (
                            "skeleton",
                            Sprite (32,32) (Right $ Animations {
                                idle = Animation { frameCount = 6, currentFrame = 1, frameSpeed = 0.3, sprites = loadAnimatedSprite "enemies/skeleton/idle.png" 6 (384,32) },
                                walk = Animation { frameCount = 10, currentFrame = 1, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/skeleton/walk.png" 10 (640,32) }
                            })
                        ),
                        (
                            "vampire",
                            Sprite (32,32) (Right $ Animations {
                                idle = Animation { frameCount = 6, currentFrame = 1, frameSpeed = 0.3, sprites = loadAnimatedSprite "enemies/vampire/idle.png" 6 (384,32) },
                                walk = Animation { frameCount = 8, currentFrame = 1, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/vampire/walk.png" 8 (512,32) }
                            })
                        ),
                        (
                            "reaper",
                            Sprite (32,32) (Right $ Animations {
                                idle = Animation { frameCount = 6, currentFrame = 1, frameSpeed = 0.3, sprites = loadAnimatedSprite "enemies/reaper/idle.png" 6 (384,32) },
                                walk = Animation { frameCount = 8, currentFrame = 1, frameSpeed = 0.1, sprites = loadAnimatedSprite "enemies/reaper/walk.png" 8 (512,32) }
                            })
                        )
                     ]
    spriteMap <- newIORef $ Map.fromList spriteList