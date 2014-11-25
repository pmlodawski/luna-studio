---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Data.Source where

import           Flowbox.Prelude
import qualified Data.ByteString as ByteString
import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8          as UTF8


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Source a = Source { _modName :: String, _src :: a}
              deriving (Show, Functor)

data Medium = File       { _path :: String     }
            | ByteString { _bs   :: ByteString }
            | String     { _str  :: String     }
            deriving (Show)

data Code = Code { _code :: ByteString }
          deriving Show

makeLenses ''Source
makeLenses ''Medium
makeLenses ''Code


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

read :: (MonadIO m, Functor m) => Source Medium -> m (Source Code)
read (Source name src) = (fmap $ Source name . Code) $ case src of
    File       path -> liftIO $ ByteString.readFile path
    ByteString bs   -> return $ bs
    String     str  -> return $ UTF8.fromString str


