module Main (main) where

import Apecs
import qualified Raylib.Core as RL
import qualified Raylib.Core.Camera as RL
import qualified Raylib.Core.Models as RL
import qualified Raylib.Core.Text as RL
import qualified Raylib.Types.Core as RL
import qualified Raylib.Types as RL
import qualified Raylib.Util as RL
import qualified Raylib.Util.Colors as RL
import Raylib.Systems as RL
import Systems as Sys
import Types
import Control.Monad (when)

main :: IO ()
main = initWorld >>= runSystem (do
    window <- RL.initialize
    Sys.initialize RL.spriteList
    settings <- get global :: System' Settings
    when (fullscreen settings) $ liftIO $ do
        RL.toggleFullscreen
        mw <- RL.getMonitorWidth 0
        mh <- RL.getMonitorHeight 0
        print (mw, mh)
        RL.setWindowSize mw mh
    liftIO $ RL.setExitKey RL.KeyBackspace
    run window
    terminate window)

