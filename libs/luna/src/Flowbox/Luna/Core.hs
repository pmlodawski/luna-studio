---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Core(
Core(..),
empty,
--loadLibrary,
--unloadLibrary,
--nodeDefByID
insertLibDef,
) where

import qualified Data.Map                                as Map
import           Data.Map                                  (Map)

import qualified Flowbox.Luna.Lib.LibManager             as LibManager
import           Flowbox.Luna.Lib.LibManager               (LibManager)
import qualified Flowbox.Luna.Lib.Library                as Library
import           Flowbox.Luna.Lib.Library                  (Library(..))
import qualified Flowbox.Luna.Network.Attributes         as Attributes
import qualified Flowbox.Luna.Network.Def.DefManager     as DefManager
import           Flowbox.Luna.Network.Def.DefManager       (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition     as Definition
import           Flowbox.Luna.Network.Def.Definition       (Definition(..))
import qualified Flowbox.Luna.Network.Flags              as Flags
import qualified Flowbox.Luna.Network.Graph.Graph        as Graph
import qualified Flowbox.Luna.Type.Type                  as Type




data Core = Core { libManager :: LibManager
                 , defManager :: DefManager
                 , lib2def    :: Map Library.ID Definition.ID
                 , def2lib    :: Map Definition.ID Library.ID
                 } deriving(Show)


empty :: Core
empty = Core LibManager.empty DefManager.empty Map.empty Map.empty


insertLibDef :: (Library.ID, Definition.ID, Library, Definition) -> Core -> Core
insertLibDef (lid, did, lib, def) core = core { libManager = libman 
                                              , defManager = defman
                                              , lib2def    = nlib2def
                                              , def2lib    = ndef2lib
                                              } where
    libman = LibManager.insNode (lid, lib) $ libManager core
    defman = DefManager.insNode (did, def) $ defManager core
    nlib2def = Map.insert lid did $ lib2def core
    ndef2lib = Map.insert did lid $ def2lib core
    




--FIXME: changed Library signature
--loadLibrary :: Core -> Library -> (Core, Library, Library.ID)
--loadLibrary (Core libManager' defManager') library = (newCore, newLibrary, libID') where
    
--    rootDefName   = Library.name library
--    [rootDefID']  = DefManager.newNodes 1 defManager'
--    rootDef       = Definition (Type.Module rootDefName) Graph.empty 
--                                Definition.noImports Flags.empty 
--                                Attributes.empty rootDefID'
--    -- TODO [PM] : load all nodes from disc
--    newDefManager = DefManager.insNode (rootDefID', rootDef) defManager'

--    [libID']      = LibManager.newNodes 1 libManager'
--    newLibrary    = library{ Library.rootDefID = rootDefID' }
--    newLibManager = LibManager.insNode (libID', newLibrary) libManager'

--    newCore       = Core newLibManager newDefManager   

--FIXME: changed Core signature
--unloadLibrary :: Core -> Library.ID -> Core
--unloadLibrary (Core libManager' defManager') libID' = newCore where
--    newLibManager = LibManager.delNode libID' libManager'
--    newDefManager = defManager' -- TODO [PM] : unload all nodes asociated with library
--    newCore       = Core newLibManager newDefManager


--nodeDefByID :: Core -> Definition.ID -> Maybe Definition
--nodeDefByID (Core _ adefManager) defID = def where
--    def = DefManager.lab adefManager defID