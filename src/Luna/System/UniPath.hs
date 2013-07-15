---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.System.UniPath(
UniPath,
empty,
fromUnixString,
toUnixString,
fromList,
append,
prepend,
toPathItem,
normalise,
fileName,
basePath
) where

import Data.List.Split (splitOn)
import Data.List (intercalate, intersperse)
-- import Data.String.Utils (join)


data PathItem = Node String | Root String | Up | Current| Empty deriving (Eq,Ord,Show)  

type UniPath = [PathItem]

-- This function is defined vecause Data.StringUtils(join) cannot be used in JS right now: https://github.com/valderman/haste-compiler/issues/63
join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

empty :: UniPath
empty = []

fromUnixString :: String -> UniPath
fromUnixString []           = empty
fromUnixString spath@(x:xs) = case x of
        '/' -> fromList $ "/" : splitOn "/" xs
        _   -> fromList $ splitOn "/" spath


toUnixString :: UniPath -> String
toUnixString path = join "/" $ fmap str path where
        str item = case item of
                Node txt -> txt
                Root txt -> txt
                Up       -> ".."
                Current  -> "."
                Empty    -> ""

fromList :: [String] -> UniPath
fromList path = foldr prepend empty path

append :: String -> UniPath -> UniPath
append snode path = path ++ [toPathItem snode]

prepend :: String -> UniPath -> UniPath
prepend snode path = (toPathItem snode):path 

toPathItem :: String -> PathItem
toPathItem snode = case snode of
        "/"  -> Root "/"
        ".." -> Up
        "."  -> Current
        ""   -> Empty
        txt  -> Node txt

normalise :: UniPath -> UniPath
normalise path = case reverse (normalise_r (reverse path) $ 0) of
        [] -> [Current]
        p  -> p

normalise_r :: UniPath -> Int -> UniPath
normalise_r path undo = case path of
        [] -> replicate undo Up
        x:xs -> case x of
                root@(Root _) -> [root]
                Up            -> normalise_r xs (undo+1)
                Current       -> normalise_r xs undo
                Empty         -> normalise_r xs undo
                _             -> if undo>0 then
                                         normalise_r xs (undo-1)
                                     else x:normalise_r xs (undo)
                

fileName :: UniPath -> String
fileName path = case last $ normalise path of
                  Node fname -> intercalate "." $ splitOn "." fname
                  _          -> error "something is wrong with the path " ++ toUnixString path

--removes the name of a file, if present
basePath :: UniPath -> UniPath
basePath path = normalise $ case last $ normalise path of
                              Node _ -> path ++ [Up]
                              _      -> path

setExtension :: String -> UniPath -> UniPath
setExtension ext path =
  normalise $ path ++ [Up] ++ [Node $ (fileName path) ++ ext]
  









