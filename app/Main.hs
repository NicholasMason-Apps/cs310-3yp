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
        play (InWindow "Dungeon Crawler" (1280, 720) (10, 10)) (makeColorI 37 19 26 255) 60 draw handleEvent step