---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Luna.Pass.Transform.Graph.GCNodeProperties.GCNodeProperties where

import           Control.Monad.State
import           Data.IntSet         ((\\))
import qualified Data.IntSet         as IntSet

import           Flowbox.Prelude                            hiding (Traversal)
import           Flowbox.System.Log.Logger
import           Luna.Pass.Analysis.ID.ExtractIDs           (EIDDefaultTraversal, EIDTraversal)
import qualified Luna.Pass.Analysis.ID.ExtractIDs           as ExtractIDs
import qualified Luna.Syntax.Enum                           as Enum
import           Luna.Syntax.Graph.PropertyMap              (PropertyMap)
import qualified Luna.Syntax.Graph.PropertyMap              as PropertyMap
import qualified Luna.Syntax.Graph.View.Default.DefaultsMap as DefaultsMap



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


gcIds :: (Enum.Enumerated a, MonadIO m,
          EIDDefaultTraversal Identity a1, EIDTraversal Identity v)
      => a1 -> PropertyMap a v -> m (PropertyMap a v)
gcIds module_ propertyMap = do
    let ids                = ExtractIDs.run module_
        existingIds        = IntSet.union ids $ IntSet.map (* (-1)) ids
        defaultsMaps       = map (flip PropertyMap.getDefaultsMap propertyMap) $ IntSet.toList existingIds
        defaults           = concatMap DefaultsMap.elems defaultsMaps
        defaultsIds        = IntSet.fromList $ map fst defaults
        defaultsContentIds = ExtractIDs.runNodeExprs (map snd defaults)
        pmIds              = PropertyMap.keysSet propertyMap
        orphans            = pmIds \\ IntSet.unions [existingIds, defaultsContentIds, defaultsIds]
    when (not $ IntSet.null orphans) $
        logger warning $ concat ["GCNodeProperties: found ", show $ IntSet.size orphans, " orphaned ids: ", show orphans]
    return $ foldr PropertyMap.delete propertyMap $ IntSet.toList orphans
