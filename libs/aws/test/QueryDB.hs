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
import qualified Flowbox.AWS.Database.Instance       as DBInstance
import qualified Flowbox.AWS.Database.Session        as DBSession
import qualified Flowbox.AWS.Database.User           as DBUser
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
    putStrLn "creating"
    Database.create db

    putStrLn "adding users"
    DBUser.add db $ User "stefan" (Password.mk "000" "ala123") 100
    DBUser.add db $ User "zenon"  (Password.mk "111" "ala123") 200
    putStrLn "getting users"
    Just u1 <- DBUser.find db "stefan"
    Just u2 <- DBUser.find db "zenon"
    Nothing <- DBUser.find db "mietek"
    print u1
    print u2

    putStrLn "adding instances"
    time <- Time.getCurrentTime
    DBInstance.add db $ Instance "a1234567" (read "1.2.3.4") time Instance.Running
    DBInstance.add db $ Instance "b2345678" (read "2.3.4.5") time Instance.Running
    DBInstance.add db $ Instance "c3456789" (read "3.4.5.6") time Instance.Running
    putStrLn "getting instances"
    Just i1 <- DBInstance.find db "a1234567"
    Just i2 <- DBInstance.find db "b2345678"
    Nothing <- DBInstance.find db "00000000"
    print i1
    print i2
    putStrLn "deleting instances"
    DBInstance.delete db ["a1234567"]
    Nothing <- DBInstance.find db "a1234567"

    let expires = Time.addUTCTime 3600 time

    putStrLn "adding sessions"
    session1 <- DBSession.create db "stefan" "b2345678" expires Session.Autocharge
    session2 <- DBSession.create db "stefan" "c3456789" expires Session.Autocharge
    print (session1, session2)

    putStrLn "finding sessions"
    userSessions <- DBSession.findByUser db "stefan"
    print $ userSessions == [session1, session2]

    putStrLn "deleting sessions"
    DBSession.deleteByID db (session1 ^. Session.id)

    putStrLn "finding sessions"
    userSessions2 <- DBSession.findByUser db "stefan"
    print $ userSessions2 == [session2]

    putStrLn "finding free instances"
    a <- DBInstance.findFree db
    print a

    putStrLn "quitting"

