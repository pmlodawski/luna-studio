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

module Luna.System.Env where

import           Flowbox.Prelude

import qualified Control.Monad.State          as State
import           Control.Monad.State.Generate (newState)
import           Data.List.Split              (splitOn)
import           Data.Map                     (Map)
import qualified Luna.System.Config           as Config
import           Luna.System.Pragma           hiding (isEnabled, lookup)
import qualified Luna.System.Pragma           as Pragma
import qualified System.Environment           as Env
import           Text.Parser.Char             (CharParsing, noneOf, string)
import           Text.Parser.Token

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
