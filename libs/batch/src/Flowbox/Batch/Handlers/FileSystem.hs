---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Handlers.FileSystem (
	ls,
	stat,

	mkdir,
	touch,
	rm,
	cp,
	mv,
) 
where

import qualified System.Directory              as Directory

import qualified Flowbox.Batch.FileSystem.Item as Item
import           Flowbox.Batch.FileSystem.Item   (Item(..))
import qualified Flowbox.System.UniPath        as UniPath
import           Flowbox.System.UniPath          (UniPath)


------ public api -------------------------------------------------

ls :: UniPath -> IO [Item]
ls path = do
	putStrLn "Not Implemented. Sorry."
	return []


stat :: UniPath -> IO Item
stat path = do
	putStrLn "Not Implemented. Sorry."
	return $ File path 0


mkdir :: UniPath -> IO ()
mkdir path = do
	putStrLn "Not Implemented. Sorry."


touch :: UniPath -> IO ()
touch path = do
	putStrLn "Not Implemented. Sorry."


rm :: UniPath -> IO ()
rm path = do
	putStrLn "Not Implemented. Sorry."


cp :: UniPath -> UniPath -> IO ()
cp src dst = Directory.copyFile (UniPath.toUnixString src) (UniPath.toUnixString dst)


mv :: UniPath -> UniPath -> IO ()
mv src dst = do
	putStrLn "Not Implemented. Sorry."
