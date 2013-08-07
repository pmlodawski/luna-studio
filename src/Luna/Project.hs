---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Project(
Project(..),
empty,
loadLibrary,
unloadLibrary,
nodeDefByID
) where

import qualified Luna.Lib.LibManager         as LibManager
import           Luna.Lib.LibManager           (LibManager)
import qualified Luna.Lib.Library            as Library
import           Luna.Lib.Library              (Library(..))
import qualified Luna.Network.Attributes     as Attributes
import qualified Luna.Network.Def.DefManager as DefManager
import           Luna.Network.Def.DefManager   (DefManager)
import qualified Luna.Network.Def.Definition as Definition
import           Luna.Network.Def.Definition   (Definition(..))
import qualified Luna.Network.Flags          as Flags
import qualified Luna.Network.Graph.Graph    as Graph
import qualified Luna.Type.Type              as Type


data Project = Project {
    libManager :: LibManager,
    defManager :: DefManager
} deriving(Show)

empty :: Project
empty = Project LibManager.empty DefManager.empty


loadLibrary :: Project -> Library -> (Project, Library, Library.ID)
loadLibrary (Project libManager' defManager') library = (newProject, newLibrary, libID') where
    
    rootDefName  = Library.name library
    [rootDefID'] = DefManager.newNodes 1 defManager'
    rootDef      = Definition (Type.Module rootDefName) Graph.empty 
                               Definition.noImports Flags.empty 
                               Attributes.empty rootDefID'
    -- TODO [PM] : load all nodes from disc
    newDefManager   = DefManager.insNode (rootDefID', rootDef) defManager'

    [libID']        = LibManager.newNodes 1 libManager'
    newLibrary      = library{ Library.rootDefID = rootDefID' }
    newLibManager   = LibManager.insNode (libID', newLibrary) libManager'

    newProject         = Project newLibManager newDefManager    


unloadLibrary :: Project -> Library.ID -> Project
unloadLibrary (Project libManager' defManager') libID' = newProject where
    newLibManager = LibManager.delNode libID' libManager'
    newDefManager = defManager' -- TODO [PM] : unload all nodes asociated with library
    newProject       = Project newLibManager newDefManager


nodeDefByID :: Project -> Definition.ID -> Maybe Definition
nodeDefByID (Project _ adefManager) defID = def where
    def = DefManager.lab adefManager defID