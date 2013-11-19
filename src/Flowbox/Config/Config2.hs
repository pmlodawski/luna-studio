
module Flowbox.Config.Config2 where 

import           Flowbox.Prelude           hiding (error)

data Version = Version { major :: Int
                       , minor :: Int
                       , patch :: Int
                       , build :: String
                       , stage :: Stage
                       } deriving (Show, Eq, Ord)

data Stage = Alpha
           | Beta
           | Release
           | Final
           deriving (Show, Eq, Ord, Read)


data Config = Config { version :: Version
                     , base    :: Section
                     } deriving(Show)

data Section = Base { path :: String 
                    , bin  :: String
                    } deriving(Show)

cfg = Config { version = Version 1 0 0 "xxx" Alpha
             , base    = Base { path  = "base"
                              , bin   = (path.base) cfg ++ "/bin"
                              --, lib   = (path.base) cfg ++ "/lib"
                              --, share = (path.base) cfg ++ "/share"
                              --, pkgDb = (path.base) cfg ++ "/pkgDb"
                              }
}

main :: IO ()
main = do 
	print cfg