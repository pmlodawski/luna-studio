---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Source.Location (
    module Flowbox.Source.Location,
    module Flowbox.Source.Location.TH,
) where

import Flowbox.Prelude
import Flowbox.Source.Location.TH



type ModuleName = String

type Location = (ModuleName, Int, Int)


format :: Location -> String
format (modName, line, pos) = concat [modName, ":", show line, ":", show pos]
