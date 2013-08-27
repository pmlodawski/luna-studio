---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

--import qualified Flowbox.Luna.Samples.HelloWorld       as HelloWorld
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
--import Options.Applicative
--import Data.List


--data Sample   = Sample { inputs   :: [String]
--                       , verbose  :: String
--                       , dump_ast :: Bool
--                       }
--              | Hello [String]
--              | Goodbye
--              | Version
--              deriving Show



--parser :: Parser Sample
--parser = Sample
--     <$> many1(argument str ( metavar "inputs" ))
--     <*> strOption (long "verbose" <> short 'v' <> help "Verbose level" <> value "0")
--     <*> switch (long "dump-ast" <> hidden)

--many1 p = (:) <$> p <*> many p

--run :: Sample -> IO ()
--run x = print x

--opts :: ParserInfo Sample
--opts = info (helper <*> parser) idm

--main :: IO ()
--main = execParser opts >>= run

--------------------------------


import           Prelude                   hiding (error)
import           Flowbox.System.Log.Logger   


logger :: (String -> IO ()) -> IO ()
logger = getLogger "MyApp.BuggyComponent"

main :: IO ()
main = do
        logger.setLevel  $ DEBUG
        logger.debug     $ "debug"
        logger.info      $ "info"
        logger.notice    $ "notice"
        logger.warning   $ "warning"
        logger.error     $ "error"
        logger.critical  $ "critical"
        logger.alert     $ "alert"
        logger.emergency $ "emergency"
