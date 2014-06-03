---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main where

import qualified Data.Time                  as Time
import           Database.PostgreSQL.Simple as PSQL

import qualified Flowbox.AWS.Database.Database       as Database
import qualified Flowbox.AWS.Database.Instance       as InstanceDB
import qualified Flowbox.AWS.Database.Session        as SessionDB
import qualified Flowbox.AWS.Database.User           as UserDB
import           Flowbox.AWS.EC2.Instance.Instance   (Instance (Instance))
import qualified Flowbox.AWS.EC2.Instance.Instance   as Instance
import qualified Flowbox.AWS.User.Password           as Password
import qualified Flowbox.AWS.User.Session            as Session
import           Flowbox.AWS.User.User               (User (User))
import           Flowbox.Prelude
import qualified Flowbox.System.Console.ASCIISpinner as Spinner


main :: IO ()
main = do
    putStr "connecting "
    db <- Spinner.runWithSpinner $ Database.mk
            $ PSQL.ConnectInfo "mydbinstance.cn1bxyb5bfdl.eu-west-1.rds.amazonaws.com"
                               5432
                               "test"
                               "kozatest123"
                               "flowbox"
    putStrLn "creating db"
    Database.create db

    putStrLn "adding users"
    UserDB.add db $ User "stefan" (Password.mk "000" "ala123") 100
    UserDB.add db $ User "zenon"  (Password.mk "111" "ala123") 200
    putStrLn "getting users"
    Just u1 <- UserDB.find db "stefan"
    Just u2 <- UserDB.find db "zenon"
    Nothing <- UserDB.find db "mietek"
    print u1
    print u2

    putStrLn "adding instances"
    time <- Time.getCurrentTime
    InstanceDB.add db $ Instance "a1234567" time Instance.Running
    InstanceDB.add db $ Instance "b2345678" time Instance.Running
    InstanceDB.add db $ Instance "c3456789" time Instance.Running
    putStrLn "getting instances"
    Just i1 <- InstanceDB.find db "a1234567"
    Just i2 <- InstanceDB.find db "b2345678"
    Nothing <- InstanceDB.find db "00000000"
    print i1
    print i2
    putStrLn "deleting instances"
    InstanceDB.delete db ["a1234567"]
    Nothing <- InstanceDB.find db "a1234567"

    let expires = Time.addUTCTime 3600 time

    putStrLn "adding sessions"
    session1 <- SessionDB.create db "stefan" "b2345678" expires Session.Autocharge
    session2 <- SessionDB.create db "stefan" "c3456789" expires Session.Autocharge
    print (session1, session2)

    putStrLn "finding sessions"
    userSessions <- SessionDB.findByUser db "stefan"
    print $ userSessions == [session1, session2]

    putStrLn "deleting sessions"
    SessionDB.deleteByID db (session1 ^. Session.id)

    putStrLn "finding sessions"
    userSessions2 <- SessionDB.findByUser db "stefan"
    print $ userSessions2 == [session2]

    putStrLn "finding free instances"
    a <- InstanceDB.findFree db
    print a

    putStrLn "quitting"

