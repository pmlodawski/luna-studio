---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.Hash where

import           Language.Haskell.Interpreter (as)
import qualified Language.Haskell.Interpreter as Interpreter

import Flowbox.Prelude
import Flowbox.System.Log.Logger
import Luna.Interpreter.Session.Data.Hash (Hash)
import Luna.Interpreter.Session.Session   (Session)



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.Session.Hash"


compute :: String -> Session (Maybe Hash)
compute varName = do
    let expr = "hash " ++ varName
    -- FIXME [PM] !!!!!!
    lift2 $ Interpreter.interpret expr (as :: Maybe Hash)
    --return $ Just $ hash varName
