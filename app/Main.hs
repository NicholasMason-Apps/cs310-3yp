module Main (main) where

import Types
import Systems
import Apecs
import Apecs.Gloss
import Draw

main :: IO ()
main = do
    w <- initWorld
    runWith w $ do
        initialize
        play (InWindow "Shmup" (1280, 720) (10, 10)) black 60 draw handleEvent step