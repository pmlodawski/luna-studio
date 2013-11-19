
module Flowbox.Config.Config2 where 
import           Flowbox.Prelude           hiding (error)



data Config = Config { root    :: String
                     , base    :: Section
                     } deriving(Show)

data Section = Base { path  :: String 
                    , bin   :: String
                    , lib   :: String
                    , share :: String
                    , pkgDb :: String
                    } deriving(Show)

mkcfg lroot = Config { root    = lroot
                     , base    = Base { path  = root        cfg ++ "/base"
                                      , bin   = (path.base) cfg ++ "/bin"
                                      , lib   = (path.base) cfg ++ "/lib"
                                      , share = (path.base) cfg ++ "/share"
                                      , pkgDb = (path.base) cfg ++ "/pkgDb"
                                      }
                     }
            where cfg = mkcfg lroot

main :: IO ()
main = do 
    let c = mkcfg "xxx"
    print c
    print "hello"