
---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Broker.Control.Handler.Handler where

import           Flowbox.Prelude                      hiding (error)
import qualified Generated.Proto.Broker.ID.New.Args   as ID_New
import qualified Generated.Proto.Broker.ID.New.Result as ID_New


class Handler h where
    newID :: h -> ID_New.Args -> IO ID_New.Result
