---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

-- FIXME[pm]: Jezeli ten pas dostarcza danych ktore sa przesylane pomiedzy passami,
-- powinien znajdowac sie w Flowbox.Luna.Data.Pass
module Luna.DEP.Graph.PropertyMap (
    module Data.IntMap,
    module Luna.DEP.Graph.PropertyMap,
) where

import           Data.IntMap
import qualified Data.IntMap as IntMap
import qualified Data.Maybe  as Maybe

import           Flowbox.Prelude                         as P hiding (set)
import qualified Luna.DEP.Graph.Attributes               as Attributes
import           Luna.DEP.Graph.Flags                    (Flags)
import qualified Luna.DEP.Graph.Node                     as Node
import           Luna.DEP.Graph.Properties               (Properties)
import qualified Luna.DEP.Graph.Properties               as Properties
import           Luna.DEP.Graph.View.Default.DefaultsMap (DefaultsMap)



type PropertyMap = IntMap Properties


getAttribute :: Node.ID -> String -> String -> PropertyMap -> Maybe String
getAttribute nodeID spaceKey key propertyMap = do
    pm <- IntMap.lookup nodeID propertyMap
    let attrs = pm ^. Properties.attrs
    Attributes.get spaceKey key attrs


setAttribute :: Node.ID -> String -> String -> String -> PropertyMap -> PropertyMap
setAttribute nodeID spaceKey key value propertyMap = IntMap.insert nodeID newProperties propertyMap where
    oldProperties = Maybe.fromMaybe def (IntMap.lookup nodeID propertyMap)
    newProperties = oldProperties & Properties.attrs
        %~ Attributes.set spaceKey key value


move :: Node.ID -> Node.ID -> PropertyMap -> PropertyMap
move current new propertyMap = case IntMap.lookup current propertyMap of
    Nothing -> propertyMap
    Just k  -> IntMap.insert new k $ IntMap.delete current propertyMap


getFlags :: Node.ID -> PropertyMap -> Flags
getFlags nodeID propertyMap = Maybe.fromMaybe def $
    view Properties.flags <$> IntMap.lookup nodeID propertyMap


setFlags :: Flags -> Node.ID -> PropertyMap -> PropertyMap
setFlags flags = modifyFlags (const flags)


modifyFlags :: (Flags -> Flags) -> Node.ID -> PropertyMap -> PropertyMap
modifyFlags fun = IntMap.alter update' where
    update' = Just . (Properties.flags %~ fun) . Maybe.fromMaybe def


getDefaultsMap :: Node.ID -> PropertyMap -> DefaultsMap
getDefaultsMap nodeID propertyMap = Maybe.fromMaybe def $
    view Properties.defaultsMap <$> IntMap.lookup nodeID propertyMap


getDefaultsMaps :: PropertyMap -> [(Node.ID, DefaultsMap)]
getDefaultsMaps = P.map (_2 %~ view Properties.defaultsMap) . IntMap.toList


modifyDefaultsMap :: (DefaultsMap -> DefaultsMap) -> Node.ID -> PropertyMap -> PropertyMap
modifyDefaultsMap fun = IntMap.alter update' where
    update' = Just . (Properties.defaultsMap %~ fun) . Maybe.fromMaybe def
