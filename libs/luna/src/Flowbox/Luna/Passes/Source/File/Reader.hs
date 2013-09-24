{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, ConstraintKinds #-}

module Flowbox.Luna.Passes.Source.File.Reader where

import           Control.Applicative         
import           Control.Monad.RWS           
import qualified System.IO                 as IO

import           Flowbox.Prelude           hiding (error, id)
import           Flowbox.Luna.Data.Source    (Source(Source))
import qualified Flowbox.System.UniPath    as UniPath
import           Flowbox.System.UniPath      (UniPath)
import           Data.String.Utils           (replace)

import qualified Flowbox.Luna.Passes.Pass  as Pass
import           Flowbox.Luna.Passes.Pass    (PassMonadIO)

import           Flowbox.System.Log.Logger   


type FRMonad m = PassMonadIO Pass.NoState m


logger :: Logger
logger = getLogger "Flowbox.Luna.Passes.Source.File.Reader"


run :: PassMonadIO s m => UniPath -> UniPath -> Pass.Result m Source
run = (Pass.runT_ (Pass.Info "FileReader") Pass.NoState) .: readSource


path2module :: UniPath -> [String]
path2module path = UniPath.toList $ UniPath.dropExtension path


getModule :: UniPath -> UniPath -> [String]
getModule rootPath path = path2module $ UniPath.makeRelative rootPath path


readSource :: FRMonad m => UniPath -> UniPath -> Pass.Result m Source
readSource rootPath path = do
    filename <- UniPath.toUnixString <$> UniPath.expand path
    txt      <- liftIO $ IO.readFile filename
    let m    = getModule rootPath path
        code = tabs2spaces txt
    return $ Source m code


tabs2spaces :: String -> String
tabs2spaces = replace "\t" "    "