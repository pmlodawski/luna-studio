---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AWS                        as AWS
import           Control.Monad              (when)
import qualified Data.Map                   as Map
import qualified Data.Text                  as Text
import           Database.PostgreSQL.Simple as PSQL
import qualified System.IO                  as IO

import qualified Flowbox.AWS.Database.Database           as Database
import qualified Flowbox.AWS.Database.Instance           as InstanceDB
import qualified Flowbox.AWS.Database.Session            as SessionDB
import qualified Flowbox.AWS.Database.User               as UserDB
import qualified Flowbox.AWS.EC2.Control.DBPool.Instance as Instance
import qualified Flowbox.AWS.EC2.Control.DBPool.Monitor  as Monitor
import qualified Flowbox.AWS.EC2.Instance.Request        as Request
import           Flowbox.AWS.Region                      (Region)
import qualified Flowbox.AWS.Region                      as Region
import qualified Flowbox.AWS.User.Password               as Password
import           Flowbox.AWS.User.User                   (User (User))
import qualified Flowbox.AWS.User.User                   as User
import qualified Flowbox.Control.Concurrent              as Concurrent
import qualified Flowbox.Control.Guard                   as Guard
import           Flowbox.Prelude                         hiding (error)
import qualified Flowbox.System.Console.ASCIISpinner     as Spinner
import           Flowbox.System.Log.Logger



rootLogger :: Logger
rootLogger = getLogger "Flowbox"


logger :: LoggerIO
logger = getLoggerIO "Flowbox.DBPool"


region :: Region
region = Region.fromText "eu-west-1"


resetDB :: PSQL.Connection -> IO ()
resetDB db = do
    putStrLn "creating db"
    Database.create db

    putStrLn "adding users"
    UserDB.add db $ User "stefan" (Password.mk "000" "ala123") 10000
    UserDB.add db $ User "zenon"  (Password.mk "111" "ala123") 20000


main :: IO ()
main = do
    rootLogger setIntLevel 5
    credential <- AWS.loadCredential
    putStr "connecting "
    db <- Spinner.runWithSpinner $ Database.mk
            $ PSQL.ConnectInfo "mydbinstance.cn1bxyb5bfdl.eu-west-1.rds.amazonaws.com"
                               5432
                               "test"
                               "kozatest123"
                               "flowbox"

    Concurrent.forkIO_ $ Guard.protect (Monitor.run credential region db) (logger error . show)

    putStrLn "enter command"
    Guard.protect (handleCmd db credential) (logger error . show)


handleCmd :: PSQL.Connection -> AWS.Credential -> IO ()
handleCmd db credential = do
    displayPrompt
    line <- words <$> getLine

    when (null line) again

    let cmd  = head line
        args = tail line

    case Map.lookup cmd commands of
        Nothing      -> putStrLn $ "Available commands: " ++ (show $ Map.keys commands)
        Just action  -> action args
    when (cmd /= "quit") again

    where
        again    = handleCmd db credential
        userName = "stefan" :: User.Name
        request  = Request.mk
        commands = Map.fromList
            [("reset-db", \_     -> resetDB db)
            ,("inst-get", \_     -> Instance.retrieve db credential region userName request >>= print)
            ,("inst-rel", \[arg] -> Instance.release db userName (Text.pack arg) >>= print)
            ,("inst",     \_     -> InstanceDB.all db >>= print)
            ,("sess",     \_     -> SessionDB.all db >>= print)
            ,("user",     \_     -> UserDB.find db userName >>= print)
            ,("users",    \_     -> UserDB.all db >>= print)
            ,("quit",     \_     -> putStrLn "Quitting...")]


displayPrompt :: IO ()
displayPrompt = putStr "> " >> IO.hFlush IO.stdout
