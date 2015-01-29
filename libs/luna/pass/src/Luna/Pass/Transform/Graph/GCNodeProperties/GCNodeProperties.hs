---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Luna.Pass.Transform.Graph.GCNodeProperties.GCNodeProperties where

import           Control.Monad.State
import           Data.IntSet         ((\\))
import qualified Data.IntSet         as IntSet

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.System.Log.Logger
import qualified Luna.Pass.Analysis.ID.ExtractIDs           as ExtractIDs
import           Luna.Pass.Pass                             (Pass)
import           Luna.Pass.Pass                             (PassMonad)
import qualified Luna.Pass.Pass                             as Pass
import qualified Luna.Syntax.Enum                           as Enum
import           Luna.Syntax.Graph.PropertyMap              (PropertyMap)
import qualified Luna.Syntax.Graph.PropertyMap              as PropertyMap
import qualified Luna.Syntax.Graph.View.Default.DefaultsMap as DefaultsMap
import           Luna.Syntax.Module                         (LModule)
import           Luna.System.Pragma.Store                   (MonadPragmaStore)



logger :: LoggerIO
logger = getLoggerIO $(moduleName)


type GCNodePropertiesPass m result = (Monad m, MonadIO m)
                                   => PassMonad Pass.NoState m result


run :: (Eq a, Enum.Enumerated a, MonadPragmaStore m, MonadIO m)
    => LModule a e -> PropertyMap a e -> EitherT Pass.PassError m (PropertyMap a e)
run = Pass.run_ (Pass.Info "GCNodeProperties") Pass.NoState .: gcIds


gcIds :: Enum.Enumerated a
      => LModule a e -> PropertyMap a e -> GCNodePropertiesPass m (PropertyMap a e)
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
        logger warning $ concat ["GCNodePropertiesPass: found ", show $ IntSet.size orphans, " orphaned ids: ", show orphans]
    return $ foldr PropertyMap.delete propertyMap $ IntSet.toList orphans
