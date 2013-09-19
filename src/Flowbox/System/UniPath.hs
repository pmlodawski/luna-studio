---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.System.UniPath(
    UniPath,
    empty,
    
    fromUnixString,
    toUnixString,
    expand,
    fromList,
    append,
    prepend,
    toPathItem,
    normalise,
    fileName,
    basePath,
    setExtension,
    dropExtension,
    dirOf,
    makeRelative,
) where

import           Control.Applicative hiding (empty)
import qualified Data.List.Split     as Split
import qualified Data.List           as List
import           Data.String.Utils     (join)
import qualified System.Directory    as Directory
import qualified System.FilePath     as FilePath



data PathItem = Node String 
              | Root String
              | Var String 
              | Up
              | Current 
              | Empty deriving (Eq,Ord,Show)  

type UniPath = [PathItem]


empty :: UniPath
empty = []

fromUnixString :: String -> UniPath
fromUnixString []           = empty
fromUnixString spath@(x:xs) = case x of
        '/' -> fromList $ "/" : Split.splitOn "/" xs
        '~' -> Var "~" : fromUnixString xs
        _   -> fromList $ Split.splitOn "/" spath


toUnixString :: UniPath -> String
toUnixString path = join "/" $ fmap str path where
        str item = case item of
                Node txt -> txt
                Root txt -> txt
                Up       -> ".."
                Current  -> "."
                Empty    -> ""
                Var v    -> v

expand :: UniPath -> IO UniPath
expand [] = return empty
expand (x:xs) = case x of
        Var "~" -> do home <- Directory.getHomeDirectory
                      rest <- expand xs
                      return $ (fromUnixString home) ++ rest
        _       -> (:) x <$> expand xs


fromList :: [String] -> UniPath
fromList path = foldr prepend empty path

append :: String -> UniPath -> UniPath
append snode path = path ++ [toPathItem snode]

prepend :: String -> UniPath -> UniPath
prepend snode path = (toPathItem snode):path 


dirOf :: UniPath -> UniPath
dirOf path = init path

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
                  Node fname -> List.intercalate "." $ Split.splitOn "." fname
                  _          -> error "something is wrong with the path " ++ toUnixString path

--removes the name of a file, if present
basePath :: UniPath -> UniPath
basePath path = normalise $ case last $ normalise path of
                              Node _ -> path ++ [Up]
                              _      -> path

setExtension :: String -> UniPath -> UniPath
setExtension ext path =
    normalise $ path ++ [Up] ++ [Node $ (fileName path) ++ ext]
  


dropExtension :: UniPath -> UniPath
dropExtension path = fromUnixString $ FilePath.dropExtension $ toUnixString path


makeRelative :: UniPath -> UniPath -> UniPath
makeRelative path1 path2 = 
    fromUnixString $ FilePath.makeRelative (toUnixString path1) (toUnixString path2)

