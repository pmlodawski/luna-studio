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

import           System.Log.Logger           
import           System.Log.Handler.Syslog   
import           System.Log.Handler.Simple   
import           System.Log.Handler          (setFormatter)
import           System.Log.Formatter        
 
-- By default, all messages of level WARNING and above are sent to stderr.
-- Everything else is ignored.

-- "MyApp.Component" is an arbitrary string; you can tune
-- logging behavior based on it later.
main = do
       debugM "MyApp.Component"  "This is a debug message -- never to be seen"
       warningM "MyApp.Component2" "Something Bad is about to happen."

       -- Copy everything to syslog from here on out.
       --s <- openlog "flowbox-batch" [PID] USER DEBUG
       --updateGlobalLogger rootLoggerName (addHandler s)
      
       errorM "MyApp.Component" "This is going to stderr and syslog."

       -- Now we'd like to see everything from BuggyComponent
       -- at DEBUG or higher go to syslog and stderr.
       -- Also, we'd like to still ignore things less than
       -- WARNING in other areas.
       -- 
       -- So, we adjust the Logger for MyApp.BuggyComponent.

       updateGlobalLogger "MyApp.BuggyComponent"
                          (setLevel DEBUG)

       -- This message will go to syslog and stderr
       debugM "MyApp.BuggyComponent" "This buggy component is buggy"

       -- This message will go to syslog and stderr too.
       warningM "MyApp.BuggyComponent" "Still Buggy"

       -- This message goes nowhere.
       debugM "MyApp.WorkingComponent" "Hello"

       -- Now we decide we'd also like to log everything from BuggyComponent at DEBUG
       -- or higher to a file for later diagnostics.  We'd also like to customize the
       -- format of the log message, so we use a 'simpleLogFormatter'

       h <- fileHandler "debug.log" DEBUG >>= \lh -> return $
                setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
       updateGlobalLogger "MyApp.BuggyComponent" (addHandler h)
      
       -- This message will go to syslog and stderr, 
       -- and to the file "debug.log" with a format like :
       -- [2010-05-23 16:47:28 : MyApp.BuggyComponent : DEBUG] Some useful diagnostics...
       debugM "MyApp.BuggyComponent" "Some useful diagnostics..."