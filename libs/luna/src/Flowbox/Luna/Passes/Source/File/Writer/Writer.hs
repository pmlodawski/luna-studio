{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.Source.File.Writer.Writer where

import           Control.Monad.RWS           
import qualified System.IO                 as IO

import           Flowbox.Prelude           hiding (error, id)
import           Flowbox.Luna.Data.Source    (Source(Source))
import qualified Flowbox.Luna.Passes.Pass  as Pass
import           Flowbox.Luna.Passes.Pass    (PassMonadIO)
import qualified Flowbox.System.Directory  as Directory
import           Flowbox.System.Log.Logger   
import qualified Flowbox.System.UniPath    as UniPath
import           Flowbox.System.UniPath      (UniPath)



type FRMonad m = PassMonadIO Pass.NoState m


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Source.File.Writer"


run :: PassMonadIO s m => UniPath -> String -> Source -> Pass.Result m ()
run rootpath = (Pass.runT_ (Pass.Info "FileWriter") Pass.NoState) .: writeSource rootpath


module2path :: [String] -> String -> UniPath
module2path m ext = UniPath.setExtension ext $ UniPath.fromList m


writeSource :: FRMonad m => UniPath -> String -> Source -> Pass.Result m ()
writeSource rootpath ext (Source m content) = do
    let path = UniPath.append (UniPath.toUnixString $ module2path m ext) rootpath
    fileName <- UniPath.expand path
    let folderName = UniPath.basePath fileName
    liftIO $ do Directory.createDirectoryIfMissing True folderName
                IO.writeFile (UniPath.toUnixString fileName) content
