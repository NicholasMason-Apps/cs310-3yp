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

main :: IO ()
main = initWorld >>= runSystem (do
    window <- RL.initialize
    Sys.initialize RL.spriteList
    liftIO $ RL.setExitKey RL.KeyBackspace
    run
    terminate window)

