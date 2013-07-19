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
load,
saveManager,
nodesByCName
) where

import qualified Data.List.Split       as Split
import qualified Data.Graph.Inductive  as DG
import qualified Data.Serialize        as DS
import qualified System.Directory      as System.Directory
import qualified Luna.Graph            as Graph
import           Luna.Graph              (Graph)
import qualified Luna.Node             as Node
import           Luna.Node               (Node)
import qualified Luna.NodeDef          as NodeDef
import qualified Data.Map              as Map
import           Data.Map                (Map)
import qualified Luna.Library          as Library
import           Luna.Library            (Library, LibID)
import qualified Luna.System.UniPath   as UniPath
import           Luna.System.UniPath     (UniPath)
--import           System.Directory.Tree as SDT
import qualified Data.ByteString       as BS
import           System.IO.Unsafe        (unsafeInterleaveIO, unsafePerformIO)
import           Control.Monad           (foldM)
import           Debug.Trace


data DefManager = DefManager{
    libraries :: Map Library.LibID Library,
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

--isFile :: SDT.DirTree a -> Bool
--isFile SDT.Dir {}    = False
--isFile SDT.File  {}  = True
--isFile SDT.Failed {} = False

lazyReadFile :: UniPath -> IO BS.ByteString
-- lazyReadFile path = unsafeInterleaveIO $ BS.readFile $
-- UniPath.toUnixString path
lazyReadFile path = unsafeInterleaveIO $ do
    let strPath = UniPath.toUnixString path
    exists <- System.Directory.doesFileExist strPath
    if exists then
      BS.readFile strPath
    else
      do
       putStrLn $ "no " ++ strPath
       return $ BS.empty


nodeToFile :: UniPath -> Node -> UniPath
nodeToFile basePath node = UniPath.append (Node.name node) basePath

getFilesToLoad :: UniPath -> Graph -> ([UniPath], [UniPath], [UniPath])
getFilesToLoad basePath gr@(Graph.Graph _ _ _ _ classes functions packages) =
  let
    mapper = map $ (nodeToFile basePath) . (Graph.nodeById gr)
  in
    (mapper $ Map.elems classes,
     mapper $ Map.elems functions,
     mapper $ Map.elems packages)

recursiveLoad :: NodeDef.NodeDef -> UniPath -> String -> DefManager -> IO DefManager
recursiveLoad ndef basePath folderName mngr =
  let newPath = UniPath.append folderName basePath
      classes, functions, packages :: [UniPath]
      (classes, functions, packages) = 
        case ndef of
          NodeDef.NotLoaded -> ([], [], [])
          NodeDef.NodeDef _ _ _ gr _ -> getFilesToLoad newPath gr
  in
    do
      m1 <- foldM (flip $ loadFile Node.ClassNode) mngr classes
      m2 <- foldM (flip $ loadFile Node.FunctionNode) m1 functions
      foldM (flip $ loadFile Node.PackageNode) m2 packages

processFile :: DefManager -> UniPath -> String -> (String -> NodeDef.NodeDef -> Node) -> BS.ByteString -> IO DefManager
processFile mngr basePath name' nodeCtor contents' =
  let
    ndef :: Either String NodeDef.NodeDef
    ndef = DS.decode contents'
  in
    case ndef of
      Left _ -> return mngr
      Right def -> 
        let
          substNode :: Node -> Node
          substNode node =
            if name' == Node.name node then
              nodeCtor name' def
            else
              node
          nodeExists :: Graph -> Bool
          nodeExists gr = any ((== name') . (Node.name) . snd) $ DG.labNodes $ Graph.repr gr
          newDefs :: Graph -> Graph
          newDefs oldGr  = oldGr{Graph.repr = DG.nmap substNode $ Graph.repr $ defs mngr}
        in
          recursiveLoad def basePath name' mngr{defs = 
            if nodeExists $ defs mngr then
              newDefs $ defs mngr
            else
              Graph.insFreshNode ( nodeCtor name' def) $ defs mngr }


load :: Library -> DefManager -> IO DefManager
load lib mngr = let
                  --manager with library
                  mngrwl =  mngr{libraries = Map.insert (newId mngr) lib $ libraries mngr}
                in
                  loadFile Node.PackageNode (Library.path lib) mngrwl

loadFile :: (String -> NodeDef.NodeDef -> Node) -> UniPath -> DefManager -> IO DefManager
loadFile ftype path manager = 
  do
    dir <- lazyReadFile $ UniPath.setExtension ".node" path
    processFile manager (UniPath.basePath path) (UniPath.fileName path) ftype dir 

--import System.Directory.Tree
--import qualified Data.Foldable as F
--import qualified Data.Traversable as T

saveNodeToFile :: UniPath -> Node -> IO ()
saveNodeToFile basePath node =
  let path = UniPath.append ((Node.name node) ++ ".node") basePath
      dir  = UniPath.append (Node.name node) basePath
  in
    do
      -- save .node file
      putStrLn (UniPath.toUnixString path)
      BS.writeFile (UniPath.toUnixString path) $ DS.encode $ Node.def node
      -- save things defined inside
      directoryExists <- System.Directory.doesDirectoryExist $ UniPath.toUnixString dir
      if not directoryExists then
         System.Directory.createDirectory $ UniPath.toUnixString dir
      else
        return ()
      saveGraph dir $ NodeDef.graph $ Node.def node

saveGraph :: UniPath -> Graph -> IO ()
saveGraph basePath graph =
    let
        nameDefs = map (Graph.nodeById graph) $ Map.elems $ Map.unions [Graph.classes graph, Graph.packages graph, Graph.functions graph]
    in
      do
        -- saveThings funcs
        -- putStrLn $ (UniPath.toUnixString basePath) ++ (show $ Graph.repr graph)
        foldM (\_ -> saveNodeToFile basePath) () nameDefs


saveManager :: UniPath -> DefManager -> IO ()
saveManager basePath manager = 
    saveGraph basePath $ defs manager
                     

nodesByCName :: String -> DefManager -> Maybe Node
nodesByCName cname manager = node where
    node = nodesByCName' (Split.splitOn "." cname) (defs manager)

nodesByCName' :: [String] -> Graph -> Maybe Node
nodesByCName' [] _ = Nothing
nodesByCName' [cname] graph = nodes where
    nodes = (Graph.childrenByName cname graph)
nodesByCName' (cname_head:cname_tail) graph = nodes where
    children = Graph.childrenByName cname_head graph
    graphs = fmap (NodeDef.graph . Node.def) children
    nodes =  (=<<) (nodesByCName' cname_tail) graphs


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
