module Main (main) where

import Types
import Systems
import Apecs
import Apecs.Gloss
import System.Random
import System.Exit
import Linear
import Control.Monad
import Data.Monoid
import Data.Semigroup (Semigroup)
import Types

main :: IO ()
main = do
    w <- initWorld
    runWith w $ do
        initialize
        play (InWindow "Shmup" (220, 360) (10, 10)) black 60 draw handleEvent step