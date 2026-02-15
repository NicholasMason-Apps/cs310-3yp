module Main (main) where

import Types
import Systems hiding (initialize)
import Gloss.Systems
import Apecs
import Apecs.Gloss
import Gloss.Draw
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef

main :: IO ()
main = do
    w <- initWorld
    runWith w $ do
        initialize
        settings <- get global :: System' Settings
        let window = if fullscreen settings then FullScreen else InWindow "Hungeon" (1280, 720) (10, 10)
        play window (makeColorI 37 19 26 255) 60 draw handleEvent step