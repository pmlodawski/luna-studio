module Luna.DefManager.DefTree(
DefTree,
TypePath,
insert,
empty
) where

import qualified Data.Map as Map
import Luna.NodeType (NodeType)


--data DefTree = DefTree NodeType (Map.Map String DefTree)

data DefTree = DefTree (Map.Map String (DefTree, NodeType)) deriving (Show)

empty :: DefTree
empty = DefTree Map.empty

type TypePath = [String]

insert :: TypePath -> NodeType -> DefTree -> DefTree
insert (name_head:[]) newType (DefTree childmap) = DefTree $ Map.insert name_head (empty, newType) childmap
insert (name_head:name_tail) newType (DefTree childmap) = DefTree newmap
	where newmap = 
		case Map.lookup name_head childmap of 
			Just (subtree, nodeType) -> 
				Map.insert name_head (insert name_tail newType subtree, nodeType) childmap
			Nothing -> error "Attempt to add node to the package that not exists!"
	
--Map.insert name_head (insert name_tail nType subtree, nType') map
	--DefTree $ Map.insert x map
	--where x = insert name_tail nType tree''
	--	--case Map.lookup name_head map of 
	--	--	Just (DefTree tree'', nType') -> 
				






--insert (name_head:name_tail) nType (DefTree map) = 
--	case Map.lookup name_head map of
--		Just (subtree, nodetype) -> insert name_tail nType subtree
--		Nothing                  -> error "error!!!"
--insert [] _ _ = error "key error"





--type TypePath = [String]
--insert :: TypePath -> NodeType -> DefTree -> DefTree
--insert (name_head:[]) nType (DefTree map) = DefTree $ Map.insert name_head (empty, nType) map
--insert (name_head:name_tail) nType (DefTree map) = 
--	case Map.lookup name_head map of
--		Just (subtree, nodetype) -> Map.insert name_head (subtree, nodetype) $ insert name_tail nType subtree
--		Nothing                  -> error "error!!!"
--insert [] _ _ = error "key error"







--module Luna.DefManager.DefTree(
--empty,
--insert
--) where

--import qualified Data.Map as Map
--import Luna.NodeType (NodeType)


----data DefTree = DefTree NodeType (Map.Map String DefTree)

--data DefTree = DefTree Map.Map String (DefTree, NodeType)

--empty = DefTree Map.empty

--type TypePath = [String]
--insert :: TypePath -> NodeType -> DefTree -> DefTree
--insert name_head:[] nType tree = Map.insert name_head (empty, nType) tree


--insert name_head:name_tail nType tree = 
--	case Map.lookup name_head tree of
--		Just (subtree, nodetype) -> let
--		      newSubtree = insert name_tail nType subtree
--		    in insert name_head (snd lkp, subtree) tree
--		Nothing  error "error!!!"



















































--type TypePath = [String]

--insert :: TypePath -> NodeType -> DefTree -> DefTree
--insert = nType 





--insert [name]                    nodeType tree = case tree of
--	DefTree Map.Map path (ntype, subtree) ->
--	Empty   ->

--insert (name_head:name_tail) nodeType tree = case Map.lookup name_head tree of
--	Just subtree -> insert name_tail nodeType subtree
--	Nothing      -> --Map.insert name_head (nodeType, )

