---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Common where


import           Control.Exception       
import           Data.Text.Lazy          (pack)
                                    
import           Batch_Types             (ArgumentException(..))
import           Flowbox.Control.Error   



throw' :: String -> c
throw' = throw . ArgumentException . Just . pack


tRunScript :: Script a -> IO a
tRunScript s = do
    e <- runEitherT s
    case e of
        Left  m -> throw' m
        Right a -> return a

