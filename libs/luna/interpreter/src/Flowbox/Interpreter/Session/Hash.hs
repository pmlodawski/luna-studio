---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Flowbox.Interpreter.Session.Hash where

import           Data.Hash                    (Hash)
import           Language.Haskell.Interpreter (as)
import qualified Language.Haskell.Interpreter as Interpreter

import Flowbox.Interpreter.Session.Session (Session)
import Flowbox.Prelude
import Flowbox.System.Log.Logger



logger :: LoggerIO
logger = getLoggerIO "Flowbox.Interpreter.Session.Hash"


compute :: String -> Session (Maybe Hash)
compute varName = do
    let expr = "hash " ++ varName
    --lift2 $ Interpreter.interpret expr (as :: Maybe Hash)
    undefined
