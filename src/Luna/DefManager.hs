---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.DefManager(
DefManager(..),
empty,
library,
load
) where

import qualified Luna.Graph   as Graph
import           Luna.Graph     (Graph)
import qualified Luna.Node    as Node
import           Luna.Node      (Node)
import qualified Luna.NodeDef as NodeDef
import qualified Data.Map     as Map
import           Data.Map       (Map)
import qualified Luna.Library as Library
import           Luna.Library   (Library, LibID)

import qualified System.UniPath as UniPath
import           System.UniPath   (UniPath)

import System.Directory.Tree   as SDT
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import qualified Data.ByteString as BS


data DefManager = DefManager{
	libraries :: Map LibID Library,
	defs      :: Graph
} deriving (Show)

empty :: DefManager
empty = DefManager Map.empty Graph.empty

library :: Node -> DefManager -> Maybe Library
library node manager = Map.lookup (NodeDef.libID $ Node.def node) $ libraries manager

newId :: DefManager -> LibID
newId manager = case Map.keys $ libraries manager of
	              []    -> 0
	              list  -> 1 + maximum list

isFile :: SDT.DirTree a -> Bool
isFile SDT.Dir {}    = False
isFile SDT.File  {}  = True
isFile SDT.Failed {} = False

collectFiles :: SDT.DirTree a -> [SDT.DirTree a]
collectFiles (Dir _ contents') = filter isFile contents' 

loadDirectory :: DirTree BS.ByteString -> DefManager -> IO DefManager
loadDirectory tree manager = let
                               processFile :: DefManager -> BS.ByteString -> DefManager
                               processFile = undefined
                               fileContents :: [BS.ByteString]
                               fileContents = map file $ collectFiles tree
                             in
                               return $ foldl processFile manager fileContents

load :: Library -> DefManager -> IO DefManager
load library' manager = do
                      dir <- readDirectoryWithL BS.readFile $ UniPath.toUnixString $ Library.path library'
                      let mngr =  manager{libraries = Map.insert (newId manager) library' $ libraries manager}
                      loadDirectory (SDT.dirTree dir) mngr

--import System.Directory.Tree
--import qualified Data.Foldable as F
--import qualified Data.Traversable as T

--main = do
--	dir <- readDirectoryWithL readFile "directory"
--	let
--		tree = dirTree dir
--		a:xs = contents tree
--		x = "oto plik:" ++ file a
--		--all_files = collectfiles tree
--		--all_filenames = collectnames2 all_files
--	--print $ all_filenames
--	print $ name a
--	_ <- getLine
--	print $ file a
