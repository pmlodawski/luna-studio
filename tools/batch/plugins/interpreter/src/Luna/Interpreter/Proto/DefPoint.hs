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

import qualified Flowbox.Batch.Project.Project                    as Project
import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Generated.Proto.Interpreter.DefPoint             as Gen
import           Luna.DEP.Data.Serialize.Proto.Conversion.Crumb   ()
import           Luna.DEP.Data.Serialize.Proto.Conversion.Library ()
import           Luna.Interpreter.Session.Data.DefPoint           (DefPoint (DefPoint))



instance Convert (Project.ID, DefPoint) Gen.DefPoint where
    encode (projectID, DefPoint libraryID bc) =
        Gen.DefPoint (encodeP projectID) (encodeP libraryID) (encode bc)
    decode (Gen.DefPoint projectID libraryID tbc) = do
        bc <- decode tbc
        return (decodeP projectID, DefPoint (decodeP libraryID) bc)
