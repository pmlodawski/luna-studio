---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Data.Source where

import           Flowbox.Prelude hiding (readFile)
import qualified Data.ByteString as ByteString
import           Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8          as UTF8
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.ByteString.Lazy (toStrict)
import           Data.Text.Lazy.IO (readFile)

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Source a = Source { _modName :: Text, _src :: a}
              deriving (Show, Functor)

data Medium = File       { _path :: Text       }
            -- | ByteString { _bs   :: ByteString }
            | Text       { _txt  :: Text       }
            deriving (Show)

data Code = Code { _code :: Text }
          deriving Show

makeLenses ''Source
makeLenses ''Medium
makeLenses ''Code


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

--read :: (MonadIO m, Functor m) => Source Medium -> m (Source Code)
--read (Source name src) = (fmap $ Source name . Code) $ case src of
--    File       path -> liftIO $ ByteString.readFile (toString path)
--    ByteString bs   -> return $ bs
--    Text       txt  -> return . toStrict $ encodeUtf8 txt


read :: (MonadIO m, Functor m) => Source Medium -> m (Source Code)
read (Source name src) = (fmap $ Source name . Code) $ case src of
    File       path -> liftIO $ readFile (toString path)
    --ByteString bs   -> return $ bs
    Text       txt  -> return txt


