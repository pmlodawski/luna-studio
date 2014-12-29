---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable        #-}


module Luna.Parser.Pragma where


import           Data.Typeable

import           Flowbox.Prelude
import           Luna.System.Session (registerPragma, enablePragma, disablePragma)
import           Luna.System.Pragma  (pragma, SwitchPragma)



data ImplicitSelf = ImplicitSelf deriving (Show, Read, Typeable)
data OrphanNames  = OrphanNames   deriving (Show, Read, Typeable)

implicitSelf = pragma :: SwitchPragma ImplicitSelf
orphanNames  = pragma :: SwitchPragma OrphanNames

init = do
    registerPragma implicitSelf
    registerPragma orphanNames

    enablePragma   implicitSelf
    disablePragma  orphanNames
