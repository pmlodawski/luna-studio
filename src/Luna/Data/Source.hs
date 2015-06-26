---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Data.Source where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as ByteString
import           Data.ByteString.Lazy    (toStrict)
import qualified Data.ByteString.UTF8    as UTF8
import qualified Data.Text.Lazy          as T
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Text.Lazy.IO       (readFile)
import           Flowbox.Prelude         hiding (Text, readFile)
import           Luna.Syntax.Name.Path   (QualPath (QualPath))


----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class SourceReader m a where
    read :: Monad m => Source a -> m (Source Code)


----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Source a = Source { _modName :: QualPath, _src :: a} deriving (Show, Functor)

newtype File = File { _path :: T.Text } deriving (Show)
newtype Code = Code { _code :: T.Text } deriving (Show)

makeLenses ''Source
makeLenses ''File
makeLenses ''Code

----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

instance (Functor m, MonadIO m) => SourceReader m File where
    read (Source name src) = fmap (Source name . Code)
                           $ liftIO $ readFile (toString $ view path src)


instance SourceReader m Code where
    read = return

