---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Luna.Interpreter.Proto.DefPoint where

import           Flowbox.Control.Error
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Interpreter.DefPoint           as Gen
import           Luna.Data.Serialize.Proto.Conversion.Crumb     ()
import           Luna.Interpreter.Session.Data.DefPoint         (DefPoint (DefPoint))


instance Convert DefPoint Gen.DefPoint where
    encode (DefPoint libraryID bc) = Gen.DefPoint tlibraryID tbc where
        tlibraryID = encodePJ libraryID
        tbc        = encodeJ  bc
    decode (Gen.DefPoint mtlibraryID mtbc) = do
        libraryID <- decodeP <$> mtlibraryID <?> "Failed to decode DefPoint: 'libraryID' field is missing"
        bc        <- decode =<< (mtbc        <?> "Failed to decode DefPoint: 'breadcrumbs' field is missing")
        return $ DefPoint libraryID bc
