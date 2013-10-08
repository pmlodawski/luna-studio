import qualified Flowbox.Config.Config as Cfg

import           Flowbox.Prelude         

main :: IO ()
main = do
    cfg <- Cfg.load
    print cfg
    return ()
