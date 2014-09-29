---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Source.Location.TH where

import Flowbox.Prelude
import Language.Haskell.TH.Syntax



loc :: Q Exp
loc = do
    Loc _ _ modName start _ <- location
    let (line, pos) = start
    return $ TupE [ LitE (StringL modName)
                  , LitE (IntegerL $ toInteger line)
                  , LitE (IntegerL $ toInteger pos)
                  ]

