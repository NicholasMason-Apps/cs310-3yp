module Main (main) where

import Types
import Systems
import Apecs
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import Draw
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef

main :: IO ()
main = do
    w <- initWorld -- Initialise Apecs world

    -- Initialize SDL
    SDL.initialize [SDL.InitVideo]
    SDL.Font.initialize

    -- Create window and renderer
    window <- SDL.createWindow "Dungeon Crawler" SDL.defaultWindow
    render <- SDL.createRenderer window (-1) SDL.defaultRenderer
    
    -- Initialize systems
    runSystem initialize world

    SDL.showWindow window

    -- Loop code
    

    SDL.destroyRenderer render
    SDL.destroyWindow window
    SDL.Image.quit
    SDL.Font.quit
    SDL.quit
    exitSuccess
    -- runWith w $ do
    --     initialize
    --     play (InWindow "Dungeon Crawler" (1280, 720) (10, 10)) (makeColorI 37 19 26 255) 60 draw handleEvent step