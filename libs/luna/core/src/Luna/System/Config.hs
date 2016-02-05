-----------------------------------------------------------------------------
-- |
-- Module      :  Luna.System.Config
-- Copyright   :  (C) 2014 Flowbox
-- License     :  AllRightsReserved
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.System.Config where

import           Flowbox.Prelude

import qualified Control.Monad.State          as State
import           Control.Monad.State.Generate (newState)
import           Data.List.Split              (splitOn)
import           Luna.System.Pragma           hiding (isEnabled, lookup)
import qualified Luna.System.Pragma           as Pragma
import qualified System.Environment           as Env
import           Text.Parser.Char             (CharParsing, noneOf, string)
import           Text.Parser.Token

----------------------------------------------------------------------
-- ConfigStore
----------------------------------------------------------------------

data Env     = Env     { _pathName :: String }
                       deriving (Show)

data Sources = Sources { _sourceExts  :: [String]
                       , _modInfoExts :: [String]
                       } deriving (Show)

data Config  = Config  { _env     :: Env
                       , _sources :: Sources
                       } deriving (Show)
makeLenses ''Env
makeLenses ''Sources
makeLenses ''Config

$(newState "ConfigStore" ''Config)

-- == Utils ==

readPath :: (MonadIO m, MonadConfigStore m, Functor m) => m [String]
readPath = do
    path     <- view (env.pathName) <$> get
    envPath  <- liftIO $ Env.lookupEnv path
    return $ filter (/= "")
           $ maybe [] (splitOn ":") envPath


-- == Instances ==

instance Default Env where
    def = Env { _pathName = "LUNAPATH" }

instance Default Sources where
    def = Sources { _sourceExts  = ["luna"]
                  , _modInfoExts = ["li"]
                  }

instance Default Config where
    def = Config def def

