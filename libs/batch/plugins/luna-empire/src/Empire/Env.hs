{-# LANGUAGE TemplateHaskell #-}

module Empire.Env where

import           Flowbox.Prelude

import           Empire.Empire

data Env = Env { _empire :: EmpireEnv }
               deriving (Show)

makeLenses ''Env


instance Default Env where
    def = Env def

