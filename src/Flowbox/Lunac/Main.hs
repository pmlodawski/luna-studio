---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--import qualified Flowbox.Luna.Confs.HelloWorld       as HelloWorld
--import qualified Flowbox.Luna.Codegen.Hs.FuncGenerator as FG
--import qualified Flowbox.Luna.Codegen.Hs.DefGenerator  as DG
--import qualified Flowbox.Luna.Codegen.Hs.CodeGenerator as CG
--import qualified Flowbox.Luna.Network.Def.DefManager   as DefManager

--import qualified Flowbox.Luna.Codegen.Hs.AST.Function  as Function
--import qualified Flowbox.Luna.Codegen.Hs.AST.Module    as Module

--import           Flowbox.Luna.Codegen.Hs.Cabal.Config    (Config)
--import qualified Flowbox.Luna.Codegen.Hs.Cabal.Config  as Config
--import qualified Flowbox.Luna.Codegen.Hs.Cabal.Section as Section

--import qualified Flowbox.Luna.Network.Graph.Graph      as Graph

--import           Flowbox.Luna.Data.Graph                 


--------------------------------
import           Options.Applicative         hiding (info)
import qualified Options.Applicative         as Opt
import           Data.List                     
import           Prelude                     hiding (error)
import qualified Flowbox.Data.Version        as Version 
import           Flowbox.Control.Applicative   
import           Flowbox.System.Log.Logger     
import qualified System.Environment          as Env
--import           Control.Error
import           Flowbox.Control.Exception

logger :: Logger
logger = getLogger "Flowbox.Lunac"

libPathEnv :: String
libPathEnv = "LUNAPATH"

version :: Version.Version
version = Version.mk { Version.minor = 1
                     , Version.stage = Version.Alpha
                     }


data Conf   = Conf { inputs   :: [String]
                   , verbose  :: Bool
                   , noColor  :: Bool
                   , dump_ast :: Bool
                   }
          | Hello [String]
          | Goodbye
          | Version
          deriving Show



parser :: Parser Conf
parser = Conf
     <$> many1(argument str ( metavar "inputs" ))
     -- <*> strOption (long "verbose"  <> short 'v' <> value "0" <> help "Verbose level")
     <*> switch    (long "verbose"  <> short 'v'                 <> help "Verbose level")
     <*> switch    (long "no-color"                              <> help "Disable color output")
     <*> switch    (long "dump-ast" <> hidden)

opts :: ParserInfo Conf
opts = Opt.info (helper <*> parser) idm

main :: IO ()
main = execParser opts >>= run

----run :: Conf -> IO ()
--run conf = runScript $ do
--  lunapath <- scriptIO $ Env.getEnv "LUNAPATH"
--  n   <- tryRead "Read failed" lunapath
--  --print lunapath
--  --print conf
--  return ()

run :: Conf -> IO ()
run conf = do
    case verbose conf of
        True  -> logger.setLevel $ DEBUG
        False -> logger.setLevel $ INFO

    logger.debug $ "Searching for environment variable '" ++ libPathEnv ++ "'"

    lunapath <- try $ Env.getEnv libPathEnv
    case lunapath of
        Left _    -> logger.debug $ "Not defined"
        Right var -> logger.debug $ "Found: '" ++ var ++ "'"
    return ()
--------------------------------

--main :: IO ()
--main = do
--        logger.setLevel  $ DEBUG
--        logger.debug     $ "debug"
--        logger.info      $ "info"
--        logger.notice    $ "notice"
--        logger.warning   $ "warning"
--        logger.error     $ "error"
--        logger.critical  $ "critical"
--        logger.alert     $ "alert"
--        logger.emergency $ "emergency"
