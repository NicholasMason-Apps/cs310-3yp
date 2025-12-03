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
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    
    -- Initialize systems
    runSystem initialize world

    SDL.showWindow window

    -- Loop code
    let loop prevTicks secondTick fpcAcc prevFps = do
            ticks <- SDL.ticks
            payload <- map SDL.eventPayload <$> SDL.pollEvents
            let quit = SDL.QuitEvent `elem` payload
                dt = ticks - prevTicks
                calcFps = secondTick + dt > 1000
                newFps = if calcFps then fpcAcc + 1 else fpcAcc
                newFpsAcc = if calcFps then 1 else fpsAcc + 1
                newSecondTick = if calcFps then mod (secondTick + dt) 1000 else secondTick + dt
            
            -- handle input events
            runSystem (handlePayload payload) world

            -- update game state
            runSystem (step $ fromIntegral dt) world

            -- Set background colour and clear the screen
            SDL.rendererDrawColor renderer SDL.$= SDL.V4 37 19 26 255
            SDL.clear renderer

            -- render the current frame
            join $ runSystem (draw renderer newFps) world
            SDL.present renderer
            unless quit $ loop ticks newSecondTick newFpsAcc newFps
    loop 0 0 0 0

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.Image.quit
    SDL.Font.quit
    SDL.quit
    exitSuccess
    -- runWith w $ do
    --     initialize
    --     play (InWindow "Dungeon Crawler" (1280, 720) (10, 10)) (makeColorI 37 19 26 255) 60 draw handleEvent step