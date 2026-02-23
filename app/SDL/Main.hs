module Main (main) where

import Types
import Systems hiding (initialize)
import Apecs
import qualified SDL
import qualified SDL.Font
import qualified SDL.Image
import SDL.Draw
import SDL.Systems
import qualified Data.Text as T
import Linear
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Control.Monad ( unless )
import System.Exit (exitSuccess)
import Control.Monad (when)

main :: IO ()
main = do
    world <- initWorld -- Initialise Apecs world

    -- Initialize SDL
    SDL.initialize [SDL.InitVideo]
    SDL.Font.initialize
    SDL.Image.initialize []

    -- Create window and renderer
    let windowConfig = SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 1280 720, SDL.windowMode = SDL.Windowed, SDL.windowHighDPI = True, SDL.windowResizable = False }
    window <- SDL.createWindow (T.pack "Hungeon") windowConfig
    
    let rendererConfig = SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer, SDL.rendererTargetTexture = False }
    renderer <- SDL.createRenderer window (-1) rendererConfig
    
    -- Initialize systems
    runSystem (initialize windowConfig renderer) world

    runSystem (do
        settings <- get global :: System' Settings
        when (fullscreen settings) $ do
            liftIO $ SDL.setWindowMode window SDL.FullscreenDesktop
            viewport <- SDL.get (SDL.rendererViewport renderer)
            let (w',h') = case viewport of
                    Just (SDL.Rectangle _ (SDL.V2 w h)) -> (w, h)
                    Nothing -> (1280, 720)
            (SDL.$=) (SDL.rendererScale renderer) (V2 (fromIntegral w' / 1280) (fromIntegral h' / 720))
            modify global $ \(Viewport _) -> Viewport (fromIntegral w', fromIntegral h')
        ) world

    SDL.showWindow window

    -- Loop code
    let loop prevTicks secondTick fpsAcc prevFps = do
            ticks <- SDL.ticks
            payload <- map SDL.eventPayload <$> SDL.pollEvents
            let quit = SDL.QuitEvent `elem` payload
                dt = ticks - prevTicks
                calcFps = secondTick + dt > 1000
                newFps = if calcFps then fpsAcc + 1 else fpsAcc
                newFpsAcc = if calcFps then 1 else fpsAcc + 1
                newSecondTick = if calcFps then mod (secondTick + dt) 1000 else secondTick + dt
            
            -- handle input events
            runSystem (handlePayload payload) world

            -- update game state
            runSystem (step $ fromIntegral dt / 1000) world
            
            -- Set background colour and clear the screen
            SDL.rendererRenderTarget renderer SDL.$= Nothing
            SDL.rendererDrawColor renderer SDL.$= SDL.V4 37 19 26 255
            SDL.clear renderer

            -- render the current frame
            runSystem (draw renderer newFps) world
            SDL.present renderer
            unless quit $ loop ticks newSecondTick newFpsAcc newFps
    loop 0 0 0 0

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.Image.quit
    SDL.Font.quit
    SDL.quit
    exitSuccess