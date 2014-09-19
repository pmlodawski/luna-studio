---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.Interpreter.Session.Error where

import           Control.Exception.Base (SomeException)
import           Control.Monad.IO.Class (MonadIO)
import qualified HscTypes

import           Flowbox.Prelude           hiding (error)
import           Flowbox.Source.Location   (Location)
import qualified Flowbox.Source.Location   as Location
import           Flowbox.System.Log.Logger


type ErrorStr = String

data Error = RunError          { _location :: Location , _exception :: SomeException        }
           | SourceError       { _location :: Location , _sourceErr :: HscTypes.SourceError }
           | ASTLookupError    { _location :: Location , _errStr    :: ErrorStr             }
           | CacheError        { _location :: Location , _errStr    :: ErrorStr             }
           | CallbackError     { _location :: Location , _exception :: SomeException        }
           | ConfigError       { _location :: Location , _errStr    :: ErrorStr             }
           | GraphError        { _location :: Location , _errStr    :: ErrorStr             }
           | NameResolverError { _location :: Location , _errStr    :: ErrorStr             }
           | PassError         { _location :: Location , _errStr    :: ErrorStr             }
           | OtherError        { _location :: Location , _errStr    :: ErrorStr             }
           deriving (Show)

makeLenses(''Error)


format :: Error -> String
format err = Location.format (err ^. location) ++ " : " ++ case err of
    RunError      _ e -> show e
    CallbackError _ e -> show e
    SourceError   _ s -> show s
    _                 -> err ^. errStr



logErrors :: MonadIO m => LoggerIO -> Either Error a -> m ()
logErrors logger result = case result of
    Left err -> logger error $ format err
    _        -> return ()
