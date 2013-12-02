{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Distribution.M where

import           Flowbox.Prelude                        
import qualified Distribution.Client.Setup            as Setup
import           Distribution.Client.Setup              (GlobalFlags)
import           Data.Monoid                            
import           Distribution.Simple.Compiler           (PackageDB(GlobalPackageDB, SpecificPackageDB))
import qualified Flowbox.Config.Config                as Config
import           Flowbox.Config.Config                  (Config)
import           Distribution.Verbosity               as Verbosity
import           Distribution.Client.Sandbox          as Sandbox
import           Distribution.Simple.Setup              (Flag(Flag))
import qualified Distribution.Client.List             as List
import qualified Distribution.Client.Config           as CabalConf

import qualified Distribution.ModuleName              as ModuleName
--import           Distribution.ModuleName        (ModuleName)

import qualified Flowbox.Distribution.Package.Package as Package
import           Flowbox.Distribution.Package.Package   (Package(Package))

import           Data.Aeson                             
import           Data.Aeson.TH                          
import           Data.Char                              (toLower)

import           GHC.Generics                           


import qualified Flowbox.Data.Version                 as Version


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

defaultGlobalFlags :: Config -> GlobalFlags
defaultGlobalFlags cfg = mempty { Setup.globalConfigFile = Flag $ (Config.cabal . Config.config) cfg }

main :: IO ()
main = do
    cfg <- Config.load
    let verbosity   = Verbosity.normal
        pkgDBs      = localPkgStack cfg
        globalFlags = defaultGlobalFlags cfg
    (_, cabalCfg) <- Sandbox.loadConfigOrSandboxConfig verbosity globalFlags mempty
    let
        configFlags  = CabalConf.savedConfigureFlags cabalCfg
        globalFlags' = CabalConf.savedGlobalFlags    cabalCfg `mappend` globalFlags
    (comp, _, conf) <- configCompilerAux' configFlags
    pkgs            <- List.getPkgList verbosity
                                       (configPackageDB' configFlags)
                                       (Setup.globalRepos globalFlags')
                                       comp
                                       conf
                                       mempty
                                       ["xxx"]

    return ()

    --print $ encode (Package "ala" (Version.Version [0,0,0] Version.Alpha))


--data Package = Package { name :: String 
--                       } deriving (Show, Generic)



