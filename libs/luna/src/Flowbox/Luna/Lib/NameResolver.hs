---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Lib.NameResolver(
    resolveDefinition
) where

import           Data.List.Split                       (splitOn)
import           Data.Maybe                            (listToMaybe, mapMaybe)
import           Flowbox.Prelude                       
import qualified Flowbox.Luna.Lib.LibManager         as LibManager
import           Flowbox.Luna.Lib.LibManager           (LibManager)
import qualified Flowbox.Luna.Lib.Library            as Library
import           Flowbox.Luna.Lib.Library              (Library)
import qualified Flowbox.Luna.Network.Def.DefManager as DefManager
import           Flowbox.Luna.Network.Def.DefManager   (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition as Definition
import           Flowbox.Luna.Network.Def.Definition   (Definition)
import           Flowbox.Luna.Network.Path.Import      (Import(Import))
import qualified Flowbox.Luna.Network.Path.Path      as Path
import           Flowbox.Luna.Network.Path.Path        (Path(Path))



getImports :: (Definition.ID, Definition) -> DefManager -> [Import]
getImports (defID, def) defManager = imports ++ parentImports where
    imports       = Definition.imports def
    parentImports = case DefManager.parent defManager defID of
                        Nothing       -> []
                        Just parentID -> getImports parentID defManager


searchDefMngr :: DefManager -> Path -> (Definition.ID, Definition) -> Maybe Definition.ID
searchDefMngr defManager path (defID, def) = do
    let name = Definition.name def
    case Path.segments path of 
        []  -> return defID
        [h] -> if name == Just h
                  then return defID
                  else Nothing
        h:_ -> if name == Just h
                  then let children = DefManager.children defManager defID 
                        in case children of 
                            [] -> Nothing
                            _  -> listToMaybe $ mapMaybe (searchDefMngr defManager (Path.tail path)) children
                  else Nothing


searchLib :: Path -> (Library.ID, Library) -> Maybe (Definition.ID, Library.ID)
searchLib path (libID, library) = do 
    let defManager    = Library.defs library
    rootDef          <- DefManager.lab defManager Library.rootDefID
    defID            <- searchDefMngr defManager path (Library.rootDefID, rootDef)
    return (defID, libID)


searchLibMngr :: LibManager -> Path -> [(Definition.ID, Library.ID)] -> [(Definition.ID, Library.ID)]
searchLibMngr libManager path list = 
    list ++ (mapMaybe (searchLib path) $ LibManager.labNodes libManager)


resolveDefinition :: String -> Definition.ID -> Library.ID -> LibManager -> Either String [(Definition.ID, Library.ID)]
resolveDefinition name parentID libID libManager = case LibManager.lab libManager libID of 
    Nothing      -> Left $ "Resolve definition failed: libID = " ++ show libID ++ " not found"
    Just library -> let defManager = Library.defs library
        in case DefManager.lab defManager parentID of
            Nothing -> Left $ "Resolve definition failed: defID = " ++ show parentID ++ " not found in library (libID = " ++ show libID ++ ")"
            Just parent -> do let imports  = getImports (parentID, parent) defManager
                                  elements = splitOn "." name
                                  possiblePaths = foldr (\(Import p n) l -> if n == head elements 
                                                                                then (Path.add p (Path.fromList $ tail elements)): l
                                                                                else l) [Path elements] imports
                              return $ foldr (searchLibMngr libManager) [] possiblePaths
