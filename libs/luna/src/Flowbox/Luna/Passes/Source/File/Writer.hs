---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Rank2Types #-}

module Flowbox.Luna.Passes.Source.File.Writer where

import           Control.Monad.RWS                    
import qualified System.IO                          as IO

import           Flowbox.Prelude                    hiding (error, id)
import           Flowbox.Luna.Data.Source             (Source(Source))
import qualified Flowbox.Luna.Passes.Pass           as Pass
import           Flowbox.Luna.Passes.Pass             (Pass)
import qualified Flowbox.System.Directory.Directory as Directory
import           Flowbox.System.Log.Logger            
import qualified Flowbox.System.UniPath             as UniPath
import           Flowbox.System.UniPath               (UniPath)



type FRPass result = Pass Pass.NoState result


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Source.File.Writer"


run :: UniPath -> String -> Source -> Pass.Result ()
run = (Pass.run_ (Pass.Info "FileWriter") Pass.NoState) .:. writeSource


module2path :: [String] -> String -> UniPath
module2path m ext = UniPath.setExtension ext $ UniPath.fromList m


writeSource :: UniPath -> String -> Source -> FRPass ()
writeSource urootpath ext (Source m content) = do
    rootpath <- UniPath.expand urootpath
    let fileName   = UniPath.fromList $ (UniPath.toList rootpath) ++ (UniPath.toList $ module2path m ext)
        folderName = UniPath.basePath fileName
    liftIO $ do Directory.createDirectoryIfMissing True folderName
                IO.writeFile (UniPath.toUnixString fileName) content
