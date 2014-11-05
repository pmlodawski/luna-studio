---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

-- FIXME OR DELETEME [WD]
-- moze stworzyc ogolna klase FS (isDirectory path -> Bool) etc, dla ktorego instancje beda dla amazona etc.

{-# LANGUAGE FlexibleContexts #-}

module Flowbox.System.UniPath where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List              as List
import qualified Data.String.Utils      as StringUtils
import qualified System.Directory       as Directory
import qualified System.FilePath        as FilePath

import Flowbox.Prelude hiding (empty)



data PathItem = Node String
              | Root String
              | Var String
              | Up
              | Current
              | Empty deriving (Eq, Ord, Show, Read)

type UniPath = [PathItem]


empty :: UniPath
empty = []


fromUnixString :: String -> UniPath
fromUnixString []           = empty
fromUnixString spath@(x:xs) = let
    split a = StringUtils.split "/" $ StringUtils.replace "\\" "/" a
    in case x of
        '/' -> fromList $ "/" : split xs
        '~' -> case xs of
                  []     -> [Var "~"]
                  '/':ys -> Var "~" : fromUnixString ys
                  _      -> fromList $ split xs
        '$' -> var : rest where
               splitted = split spath
               var  = Var $ head splitted
               rest = fromList $ tail splitted
        _   -> fromList $ split spath


toUnixString :: UniPath -> String
toUnixString []   = ""
toUnixString path = case head l of
        "/" -> "/" ++ join (tail l)
        _   -> join l
    where l    = toList path
          join = StringUtils.join "/"


expand :: MonadIO m => UniPath -> m UniPath
expand [] = return empty
expand (x:xs) = liftIO $ case x of
    Var "~"           -> expandRest   Directory.getHomeDirectory
    Var "$APPFLOWBOX" -> expandRest $ Directory.getAppUserDataDirectory "flowbox"
    Var "$APPDATA"    -> expandRest   Directory.getAppDataDirectory
    Var "$TEMP"       -> expandRest   Directory.getTemporaryDirectory
    Var "$DOCUMENTS"  -> expandRest   Directory.getUserDocumentsDirectory
    _       -> (:) x <$> expand xs
    where expandRest fvar = do
              var <- fvar
              rest <- expand xs
              return $ fromUnixString var ++ rest


fromList :: [String] -> UniPath
fromList = foldr prepend empty


toList :: UniPath -> [String]
toList = fmap str where
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
prepend snode path = toPathItem snode : path


dirOf :: UniPath -> UniPath
dirOf = init


toPathItem :: String -> PathItem
toPathItem snode = case snode of
        "/"  -> Root "/"
        ".." -> Up
        "."  -> Current
        ""   -> Empty
        txt  -> Node txt


normalise :: UniPath -> UniPath
normalise path = case reverse (normaliseR (reverse path) 0) of
        [] -> [Current]
        p  -> p


normaliseR :: UniPath -> Int -> UniPath
normaliseR path undo = case path of
        []   -> replicate undo Up
        x:xs -> case x of
                   root@(Root _) -> [root]
                   Up            -> normaliseR xs (undo+1)
                   Current       -> if null xs && undo == 0
                                        then path
                                        else normaliseR xs undo
                   Empty         -> normaliseR xs undo
                   _             -> if undo > 0
                                        then normaliseR xs (undo-1)
                                        else x:normaliseR xs undo


fileName :: UniPath -> String
fileName path = case last $ normalise path of
                  Node fname -> List.intercalate "." $ StringUtils.split "." fname
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
    normalise $ path ++ [Up] ++ [Node $ fileName path ++ ext]


dropExtension :: UniPath -> UniPath
dropExtension path = fromUnixString $ FilePath.dropExtension $ toUnixString path


makeRelative :: UniPath -> UniPath -> UniPath
makeRelative path1 path2 =
    fromUnixString $ FilePath.makeRelative (toUnixString path1) (toUnixString path2)
