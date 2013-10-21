---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.System.UniPath where

import           Control.Applicative    hiding (empty)
import qualified Data.List.Split        as Split
import qualified Data.List              as List
import qualified Data.String.Utils      as StringUtils
import qualified System.Directory       as Directory
import qualified System.FilePath        as FilePath
import           Control.Monad.IO.Class   (MonadIO, liftIO)

import           Flowbox.Prelude          


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
fromUnixString spath@(x:xs) = let 
    split = Split.splitOn "/"
    in case x of
        '/' -> fromList $ "/" : split xs
        '~' -> case xs of 
                  []     -> [Var "~"]
                  '/':ys -> Var "~" : (fromUnixString ys)
                  _      -> fromList $ split xs
        '$' -> var : rest where
               splitted = split spath
               var  = Var $ head splitted
               rest = fromList $ tail splitted
        _   -> fromList $ split spath


toUnixString :: UniPath -> String
toUnixString path = StringUtils.join "/" $ toList path


expand :: MonadIO m => UniPath -> m UniPath
expand [] = return empty
expand (x:xs) = liftIO $ case x of
        Var "~"        -> do home <- Directory.getHomeDirectory
                             rest <- expand xs
                             return $ (fromUnixString home) ++ rest
        Var "$APPDATA" -> do home <- Directory.getAppUserDataDirectory "flowbox"
                             rest <- expand xs
                             return $ (fromUnixString home) ++ rest
        _       -> (:) x <$> expand xs


fromList :: [String] -> UniPath
fromList path = foldr prepend empty path


toList :: UniPath -> [String]
toList path = fmap str path where
    str item = case item of
            Node txt -> txt
            Root txt -> txt
            Up       -> ".."
            Current  -> "."
            Empty    -> ""
            Var v    -> v


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


extension :: UniPath -> String
extension path = FilePath.takeExtension (toUnixString path)


setExtension :: String -> UniPath -> UniPath
setExtension ext path =
    normalise $ path ++ [Up] ++ [Node $ (fileName path) ++ ext]
  

dropExtension :: UniPath -> UniPath
dropExtension path = fromUnixString $ FilePath.dropExtension $ toUnixString path


makeRelative :: UniPath -> UniPath -> UniPath
makeRelative path1 path2 = 
    fromUnixString $ FilePath.makeRelative (toUnixString path1) (toUnixString path2)