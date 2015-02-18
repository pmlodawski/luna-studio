---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------

module Luna.Renderer.Data.FileNamePattern where

import Flowbox.Prelude



type FileNamePattern = String


toFileName :: FileNamePattern -> Int -> FilePath
toFileName = const --FIXME[PM]
