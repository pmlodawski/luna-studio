---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Arg where

import Data.Binary  (Binary)
import GHC.Generics (Generic)

import Flowbox.Prelude
import Luna.Syntax.Label (Label)
import Luna.Syntax.Pat   (LPat)


data Arg  a v = Arg { _pat :: LPat a, _val :: Maybe v } deriving (Show, Generic, Eq, Read)
type LArg a v = Label a (Arg a v)


makeLenses ''Arg


instance (Binary a, Binary v) => Binary (Arg a v)
