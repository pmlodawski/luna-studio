{-# LANGUAGE TemplateHaskell #-}

module Empire.Env where

import           Flowbox.Prelude

import qualified Empire.Empire    as Empire

data Env = Env { _empireEnv :: Empire.Env }
               deriving (Show)

makeLenses ''Env

instance Default Env where
    def = Env def

