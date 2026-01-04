module Input where

import qualified Data.Map as M
import qualified Data.Set as Set
import Types

-- Bool in type is True for pressed, False for released
updateKeySet :: Ord rawKey => KeyBindings rawKey -> rawKey -> Bool -> Set.Set GameKey -> Set.Set GameKey
updateKeySet (KeyBindings m) rk pressed s = case M.lookup rk m of
    Nothing -> s
    Just key -> if pressed then
                    Set.insert key s
                else
                    Set.delete key s
