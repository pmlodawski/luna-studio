module Flowbox.RepoManager.VCS.Type where

import           Flowbox.Prelude

data Type = Git deriving (Show)

data VCS = VCS { type_ :: Type
               , local :: String  
               , name :: String  
               , remote :: String
               } deriving (Show)   