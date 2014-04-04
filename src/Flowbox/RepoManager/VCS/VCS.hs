module Flowbox.RepoManager.VCS.VCS where

import qualified Data.List                    as List
import           Flowbox.Prelude
import qualified Flowbox.RepoManager.VCS.Type as VCS
import           System.FilePath              (pathSeparator)
import           System.Process               (ProcessHandle)


data VCS = VCS { cls        :: VCS.Type
               , name       :: String
               , localPath  :: String
               , remotePath :: String
               , clone      :: VCS -> IO VCS
               , update     :: VCS -> IO VCS
               , remove     :: VCS -> IO ProcessHandle
               }


instance  Show VCS where
    show (VCS cls name localPath remotePath _ _ _) = "VCS {type = " ++ (show cls)
                                                     ++ ", name = " ++ (show name)
                                                     ++ ", path = " ++ (show localPath)
                                                     ++ ", uri = "  ++ (show remotePath)
                                                     ++ "}"