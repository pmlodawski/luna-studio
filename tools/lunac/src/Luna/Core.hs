---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Core(
Core(..),
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
import qualified Luna.Network.Def.NodeDef    as NodeDef
import           Luna.Network.Def.NodeDef      (NodeDef(..))
import qualified Luna.Network.Flags          as Flags
import qualified Luna.Network.Graph.Graph    as Graph
import qualified Luna.Type.Type              as Type


data Core = Core {
    libManager :: LibManager,
    defManager :: DefManager
} deriving(Show)

empty :: Core
empty = Core LibManager.empty DefManager.empty


loadLibrary :: Core -> Library -> (Core, Library, Library.ID)
loadLibrary (Core libManager' defManager') library = (newCore, newLibrary, libID') where
    
    rootNodeDefName = Library.name library
    [rootNodeDefID'] = DefManager.newNodes 1 defManager'
    rootNodeDef     = NodeDef (Type.Module rootNodeDefName) Graph.empty 
                               NodeDef.noImports Flags.empty 
                               Attributes.empty rootNodeDefID'
    -- TODO [PM] : load all nodes from disc
    newDefManager   = DefManager.insNode (rootNodeDefID', rootNodeDef) defManager'

    [libID']         = LibManager.newNodes 1 libManager'
    newLibrary      = library{ Library.rootNodeDefID = rootNodeDefID' }
    newLibManager   = LibManager.insNode (libID', newLibrary) libManager'

    newCore         = Core newLibManager newDefManager    


unloadLibrary :: Core -> Library.ID -> Core
unloadLibrary (Core libManager' defManager') libID' = newCore where
    newLibManager = LibManager.delNode libID' libManager'
    newDefManager = defManager' -- TODO [PM] : unload all nodes asociated with library
    newCore       = Core newLibManager newDefManager


nodeDefByID :: Core -> NodeDef.ID -> NodeDef
nodeDefByID (Core _ defManager) defID = def where
    def = DefManager.lab defManager defID