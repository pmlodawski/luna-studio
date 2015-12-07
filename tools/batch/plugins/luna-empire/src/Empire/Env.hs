{-# LANGUAGE TemplateHaskell #-}

module Empire.Env where

import Data.Map.Strict (Map)
import Data.Time.Clock (UTCTime)

import qualified Flowbox.Bus.Data.Message as Message
import           Flowbox.Prelude



data Env = Env { _times :: !(Map Message.CorrelationID UTCTime) }
               deriving (Show)

makeLenses ''Env


instance Default Env where
    def = Env def

