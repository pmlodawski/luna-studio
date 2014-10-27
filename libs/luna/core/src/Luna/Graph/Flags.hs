---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Graph.Flags where

import Flowbox.Prelude
import Luna.Graph.Node.Position (Position)



data Flags = Flags { _omit                 :: Bool
                   , _astFolded            :: Maybe Bool
                   , _astAssignment        :: Maybe Bool
                   , _graphFolded          :: Maybe Bool
                   , _grouped              :: Maybe Bool
                   , _defaultNodeGenerated :: Maybe Bool
                   , _graphViewGenerated   :: Maybe Bool
                   , _nodePosition         :: Maybe Position
                   } deriving (Show, Read, Eq)


makeLenses ''Flags


instance Default Flags where
    def = Flags False Nothing Nothing Nothing Nothing Nothing Nothing Nothing


isSet' :: Flags -> (Flags -> Maybe Bool) -> Bool
isSet' flags getter = getter flags == Just True
