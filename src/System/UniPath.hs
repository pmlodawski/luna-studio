--module System.Test(
--test
--) where

import Data.List.Split (splitOn)

main = do 
	let
		p = Node "a" : Node "b" : empty
	print p
	return ()

data PathItem = Node String | UP deriving (Eq,Ord,Show)  

type UniPath = [PathItem]

empty = [] :: UniPath

fromString :: String -> UniPath
fromString spath = fromList $ splitOn "/" spath

fromList :: [String] -> UniPath
fromList list = empty -- TODO

append :: String -> UniPath -> UniPath
append snode path = path ++ [toPathItem snode]

prepend :: String -> UniPath -> UniPath
prepend snode path = (toPathItem snode):path 

toPathItem :: String -> PathItem
toPathItem snode = case snode of
	".." -> UP
	txt  -> Node txt