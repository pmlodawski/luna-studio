---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

-- FIXME[pm]: Jezeli ten pas dostarcza danych ktore sa przesylane pomiedzy passami,
-- powinien znajdowac sie w Flowbox.Luna.Data.Pass
module Luna.Syntax.Graph.PropertyMap (
    module Data.IntMap,
    module Luna.Syntax.Graph.PropertyMap,
) where

import           Data.IntMap
import qualified Data.IntMap as IntMap
import qualified Data.Maybe  as Maybe

import           Flowbox.Prelude                     as P hiding (set)
import qualified Luna.Syntax.Graph.Attributes               as Attributes
import           Luna.Syntax.Graph.Flags                    (Flags)
import qualified Luna.Syntax.Graph.Node                     as Node
import           Luna.Syntax.Graph.Properties               (Properties)
import qualified Luna.Syntax.Graph.Properties               as Properties
import           Luna.Syntax.Graph.View.Default.DefaultsMap (DefaultsMap)



type PropertyMap a v = IntMap (Properties a v)


getAttribute :: Node.ID -> String -> String -> PropertyMap a v -> Maybe String
getAttribute nodeID spaceKey key propertyMap = do
    pm <- IntMap.lookup nodeID propertyMap
    let attrs = pm ^. Properties.attrs
    Attributes.get spaceKey key attrs


setAttribute :: Node.ID -> String -> String -> String -> PropertyMap a v -> PropertyMap a v
setAttribute nodeID spaceKey key value propertyMap = IntMap.insert nodeID newProperties propertyMap where
    oldProperties = Maybe.fromMaybe def (IntMap.lookup nodeID propertyMap)
    newProperties = oldProperties & Properties.attrs
        %~ Attributes.set spaceKey key value


move :: Node.ID -> Node.ID -> PropertyMap a v -> PropertyMap a v
move current new propertyMap = case IntMap.lookup current propertyMap of
    Nothing -> propertyMap
    Just k  -> IntMap.insert new k $ IntMap.delete current propertyMap


getFlags :: Node.ID -> PropertyMap a v -> Flags
getFlags nodeID propertyMap = Maybe.fromMaybe def $
    view Properties.flags <$> IntMap.lookup nodeID propertyMap


setFlags :: Flags -> Node.ID -> PropertyMap a v -> PropertyMap a v
setFlags flags = modifyFlags (const flags)


modifyFlags :: (Flags -> Flags) -> Node.ID -> PropertyMap a v -> PropertyMap a v
modifyFlags fun = IntMap.alter update' where
    update' = Just . (Properties.flags %~ fun) . Maybe.fromMaybe def


getDefaultsMap :: Node.ID -> PropertyMap a v -> DefaultsMap a v
getDefaultsMap nodeID propertyMap = Maybe.fromMaybe def $
    view Properties.defaultsMap <$> IntMap.lookup nodeID propertyMap


getDefaultsMaps :: PropertyMap a v -> [(Node.ID, DefaultsMap a v)]
getDefaultsMaps = P.map (_2 %~ view Properties.defaultsMap) . IntMap.toList


modifyDefaultsMap :: (DefaultsMap a v -> DefaultsMap a v) -> Node.ID -> PropertyMap a v -> PropertyMap a v
modifyDefaultsMap fun = IntMap.alter update' where
    update' = Just . (Properties.defaultsMap %~ fun) . Maybe.fromMaybe def
