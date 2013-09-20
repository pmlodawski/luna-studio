---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts, ConstraintKinds #-}

module Flowbox.Luna.Passes.FileReader.SourceReader where

import           Control.Applicative        
import           Control.Monad.RWS          
import qualified Data.List                as List
import qualified Data.List.Split          as Split
import qualified System.IO                as IO

import           Flowbox.Prelude          hiding (error, id)
import           Flowbox.Luna.Data.Source   (Source(Source))
import qualified Flowbox.System.UniPath   as UniPath
import           Flowbox.System.UniPath     (UniPath)



-- TODO [PM] : magic constants
path2module :: UniPath -> String
path2module path = 
    List.concat $ List.intersperse "." $ Split.splitOn "/" $ UniPath.toUnixString $ UniPath.dropExtension path


getModule :: UniPath -> UniPath -> String
getModule rootPath path = m where
    m = path2module $ UniPath.makeRelative rootPath path


run :: MonadIO m => UniPath -> UniPath -> m Source
run rootPath path = do
    liftIO $ readSource rootPath path


readSource :: UniPath -> UniPath -> IO Source
readSource rootPath path = do
    let m = getModule rootPath path
    filename <- UniPath.toUnixString <$> UniPath.expand path
    content <- IO.readFile filename
    return $ Source m content