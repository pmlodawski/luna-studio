---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Flowbox.Distribution.Package.PackageFamily where

import qualified Data.Aeson.TH     as JSON
import           Data.Default      (Default, def)
import           Data.Function     (on)
import           Data.List         (sortBy)
import qualified Data.Map          as Map
import           Data.String       (fromString)
import           Data.String.Utils (join)
import           GHC.Generics

import           Flowbox.Data.List                            (foldri)
import           Flowbox.Data.Version                         (Version)
import qualified Flowbox.Data.Version                         as Version
import           Flowbox.Distribution.License                 (License)
import           Flowbox.Distribution.Package.Dependency      (Dependency)
import           Flowbox.Distribution.Package.Package         (Package)
import qualified Flowbox.Distribution.Package.Package         as Package
import           Flowbox.Prelude                              hiding (print)
import           Flowbox.System.Console.StyledText.StyledText (StyledText)
import qualified Flowbox.System.Console.StyledText.StyledText as StyledText



data PackageFamily = PackageFamily { _name              :: String
                                   , _availableVersions :: [Version]
                                   , _installedVersions :: [Version]
                                   , _synopsis          :: String
                                   , _description       :: String
                                   , _homepage          :: String
                                   , _url               :: String
                                   , _bugReports        :: String
                                   , _license           :: License
                                   , _licenseFile       :: Maybe String
                                   , _authors           :: [String]
                                   , _maintainers       :: [String]
                                   , _copyright         :: String
                                   , _tags              :: [String]
                                   , _dependencies      :: [Dependency]
                                   } deriving (Show, Generic)

makeLenses (''PackageFamily)


mk :: ([Package], [Package]) -> PackageFamily
mk (srcPkgs, instPkgs) = foldri registerAvailable srcPkgs
                       $ foldri registerInstalled instPkgs
                       $ foldri update ( reverse $ sortBy (compare `on` (view $ Package.id . Package.version)) (instPkgs ++ srcPkgs)) def


update :: Package -> PackageFamily -> PackageFamily
update pkg pkgF = pkgF & name              .~ (pkg ^. Package.id ^. Package.name)
                       & synopsis          .~ (pkg ^. Package.synopsis)
                       & description       .~ (pkg ^. Package.description)
                       & homepage          .~ (pkg ^. Package.homepage)
                       & url               .~ (pkg ^. Package.url)
                       & bugReports        .~ (pkg ^. Package.bugReports)
                       & license           .~ (pkg ^. Package.license)
                       & licenseFile       .~ (pkg ^. Package.licenseFile)
                       & authors           .~ (pkg ^. Package.authors)
                       & maintainers       .~ (pkg ^. Package.maintainers)
                       & copyright         .~ (pkg ^. Package.copyright)
                       & tags              .~ (pkg ^. Package.tags)
                       & dependencies      .~ (pkg ^. Package.dependencies)


registerAvailable :: Package -> PackageFamily -> PackageFamily
registerAvailable pkg pkgF = pkgF & availableVersions %~ (pkg ^. Package.id ^. Package.version :)


registerInstalled :: Package -> PackageFamily -> PackageFamily
registerInstalled pkg pkgF = pkgF & installedVersions %~ (pkg ^. Package.id ^. Package.version :)



styleShow :: PackageFamily -> StyledText
styleShow pf =  indicator ++ " " ++ title
             ++ showSection "Synopsis:           " (fromString   $ pf ^. synopsis)
             ++ showSection "Homepage:           " (fromString   $ pf ^. homepage)
             ++ showSection "License:            " (fromString   $ show $ pf ^. license)
             ++ showSection "Available versions: " (showVersions $ pf ^. availableVersions)
             ++ showSection "Installed versions: " (showVersions $ pf ^. installedVersions)

             where nl = "\n"
                   indent      = fromString $ replicate 4 ' '
                   titleIndent = fromString $ replicate 20 ' '
                   nli         = nl ++ indent
                   isInstalled = not $ null $ pf ^. installedVersions
                   indicator   = if isInstalled then "[" ++ StyledText.green "I" ++ "]"
                                                else StyledText.green "*"
                   title       = cmod (fromString $ pf ^. name)
                                 where cmod = if isInstalled then StyledText.green else id
                   showSection prefix content  = if null content then ""
                                               else nli ++ StyledText.blue prefix ++ content
                   collectVersions vs        = Map.foldrWithKey showVersion [] $ Version.partition 2 vs
                   showVersions vs           = join (nl ++ indent ++ titleIndent) $ collectVersions vs
                   showVersion branch vs lst =  (StyledText.yellow (fromString $ "(" ++ Version.readableBranch branch ++ ") ")
                                             ++ fromString (join ", " (map Version.readable vs))) : lst


print :: PackageFamily -> IO ()
print pf =  (StyledText.print $ styleShow pf)
         *> putStrLn ""



------------------------------------------------------------------------
-- INSTANCES
------------------------------------------------------------------------

instance Default PackageFamily where
    def = PackageFamily { _name              = def
                        , _availableVersions = def
                        , _installedVersions = def
                        , _synopsis          = def
                        , _description       = def
                        , _homepage          = def
                        , _url               = def
                        , _bugReports        = def
                        , _license           = def
                        , _licenseFile       = def
                        , _authors           = def
                        , _maintainers       = def
                        , _copyright         = def
                        , _tags              = def
                        , _dependencies      = def
                        }


JSON.deriveJSON JSON.defaultOptions{JSON.fieldLabelModifier = drop 1} ''PackageFamily


