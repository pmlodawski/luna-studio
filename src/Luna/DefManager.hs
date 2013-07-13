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

import qualified Data.Graph.Inductive as DG
import qualified Data.MultiMap as MultiMap
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

import qualified Data.ByteString as BS
import qualified Data.Serialize as DS
import System.IO.Unsafe(unsafeInterleaveIO)
import Control.Monad(foldM)

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

lazyReadFile :: UniPath -> IO BS.ByteString
lazyReadFile path = unsafeInterleaveIO $ BS.readFile $ UniPath.toUnixString path

nodeToFile :: UniPath -> Node -> UniPath
nodeToFile basePath node = UniPath.append (Node.name node) basePath

getFilesToLoad :: UniPath -> Graph -> [UniPath]
getFilesToLoad basePath gr@(Graph.Graph _ _ _ classes functions packages) =
  let
    nodeToNode :: DG.Node -> Node
    nodeToNode = (flip Graph.nodeById) gr
    interestingNodes :: [DG.Node]
    interestingNodes = (Map.elems classes) ++ (concat $ MultiMap.elems functions) ++ (Map.elems packages) 
  in
    map ((nodeToFile basePath) . nodeToNode) interestingNodes 

recursiveLoad :: NodeDef.NodeDef -> UniPath -> String -> DefManager -> IO DefManager
recursiveLoad ndef basePath folderName mngr =
  let newPath = UniPath.append folderName basePath
      filesToLoad :: [UniPath]
      filesToLoad = case ndef of
                      NodeDef.NotLoaded -> []
                      NodeDef.NodeDef _ _ _ gr _ -> getFilesToLoad newPath gr
  in
    foldM (flip loadFile) mngr filesToLoad 

processFile :: DefManager -> UniPath -> String -> BS.ByteString -> IO DefManager
processFile mngr basePath name' contents' = let
                           ndef ::Either String NodeDef.NodeDef
                           ndef = DS.decode contents'
                        in
                           case ndef of
                             Left _ -> return mngr
                             Right def -> recursiveLoad def basePath name' mngr{defs = Graph.insFreshNode ( Node.PackageNode name' def) $ defs mngr}


load :: Library -> DefManager -> IO DefManager
load lib mngr = let
                  --manager with library
                  mngrwl =  mngr{libraries = Map.insert (newId mngr) lib $ libraries mngr}
                in
                  loadFile (Library.path lib) mngrwl

loadFile :: UniPath -> DefManager -> IO DefManager
loadFile path manager = do
                      dir <-lazyReadFile path
                      processFile manager (UniPath.basePath path) (UniPath.fileName path) dir 

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
