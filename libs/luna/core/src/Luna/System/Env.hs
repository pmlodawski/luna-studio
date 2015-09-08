-----------------------------------------------------------------------------
-- |
-- Module      :  Luna.System.Config
-- Copyright   :  (C) 2014 Flowbox
-- License     :  AllRightsReserved
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}

module Luna.System.Env where

import Flowbox.Prelude

import qualified Luna.System.Pragma           as Pragma
import           Luna.System.Pragma           hiding (lookup, isEnabled)
import qualified Control.Monad.State          as State
import           Text.Parser.Char             (string, noneOf, CharParsing)
import           Text.Parser.Token
import           Control.Monad.State.Generate (newState)
import           Data.List.Split              (splitOn)
import qualified System.Environment           as Env
import           Data.Map                     (Map)
import qualified Luna.System.Config           as Config

----------------------------------------------------------------------
-- ConfigStore
----------------------------------------------------------------------

data Env = Env { _repos :: [String] }
           deriving (Show)

makeLenses ''Env

$(newState "EnvState" ''Env)

-- == Instances ==

instance (MonadIO m, Functor m, Config.MonadConfigStore m) => DefaultM m Env where
    defM = Env <$> do
        path    <- view (Config.env . Config.pathName) <$> Config.get
        envPath <- liftIO $ Env.lookupEnv path
        return $ filter (/= "")
               $ maybe [] (splitOn ":") envPath
