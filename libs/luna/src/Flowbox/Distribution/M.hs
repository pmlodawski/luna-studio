module Flowbox.Distribution.M where

import           Flowbox.Prelude                
import           Distribution.Client.Setup      (GlobalFlags)
import           Data.Monoid                    
import           Distribution.Simple.Compiler   (PackageDB(GlobalPackageDB, SpecificPackageDB))
import qualified Flowbox.Config.Config        as Config
import           Flowbox.Config.Config          (Config)
import           Distribution.Verbosity       as Verbosity
import           Distribution.Client.Sandbox  as Sandbox


localPkgDB :: Config -> PackageDB
localPkgDB = SpecificPackageDB . Config.pkgDb . Config.local

globalPkgDB :: Config -> PackageDB
globalPkgDB = SpecificPackageDB . Config.pkgDb . Config.global

localPkgStack :: Config -> [PackageDB]
localPkgStack cfg = [ GlobalPackageDB
                    , localPkgDB  cfg
                    , globalPkgDB cfg
                    ]

globalPkgStack :: Config -> [PackageDB]
globalPkgStack cfg = [ GlobalPackageDB
                     , globalPkgDB cfg
                     , localPkgDB  cfg
                     ]

main :: IO ()
main = do
    let flags :: GlobalFlags
        flags = mempty
    cfg <- Config.load
    let pkgDBs = localPkgStack cfg
    (_, cabalCfg) <- Sandbox.loadConfigOrSandboxConfig Verbosity.normal mempty mempty
    --print cabalCfg
    print pkgDBs
