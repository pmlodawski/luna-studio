

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FunctionalDependencies            #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE DeriveDataTypeable            #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverlappingInstances            #-}
{-# LANGUAGE ViewPatterns            #-}
{-# LANGUAGE UndecidableInstances            #-}
{-# LANGUAGE TypeFamilies            #-}

{-# LANGUAGE NoOverloadedStrings            #-}

{-# LANGUAGE CPP #-}

module Flowbox.System.Path2 where

--import           Control.Monad      ((>=>))
import           Data.Binary        (Binary)
import           Data.Typeable
import           Data.List.Split    (splitOn)
--import qualified Data.List          as List
--import qualified Data.String.Utils  as StringUtils
import qualified System.Directory   as Directory
import qualified System.Environment as Env
import           Control.Exception  (catch, SomeException)
import           Control.Monad      (join)
--import           System.IO.Error    (IOError, isDoesNotExistError)
import           System.IO.Error    (tryIOError)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Control.Error.Safe (justErr)
import           Data.Binary

--import           Flowbox.Prelude                    hiding (empty, fromList, toList)
--import qualified Flowbox.System.Directory.Locations as Directory
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Unsafe.Coerce (unsafeCoerce)
import Flowbox.Prelude
import GHC.TypeLits

import Control.Monad.Shuffle (deepBind, (>>>=), ($>>=))

import Data.ConcreteTypeRep

import qualified Data.Map as Map
import Control.Monad.Trans.Either
import Data.List (intercalate)

#include "ghcplatform.h"


------------------------------------------------------------------------
-- Path
------------------------------------------------------------------------

newtype Path seg = Path [seg] deriving (Functor, Monoid, Monad)

data Segment = Root
             | Node String
             deriving (Show, Eq, Generic, Typeable)

data RelSegment = Var String
                | Special SpecialPath
                | Up
                | Segment Segment
                deriving (Show, Generic)

data SpecialPath = forall a. (ResolvePlatform a, Show a) => SpecialPath a

type AbsPath = Path Segment
type RelPath = Path RelSegment

type IsRelPath a = Convertible a RelPath
type IsAbsPath a = Convertible a AbsPath

-- === utils ===

segments :: Path seg -> [seg]
segments (Path a) = a

specialPath :: (ResolvePlatform a, Show a) => a -> RelPath
specialPath = Path . return . Special . SpecialPath

toAbsPath :: IsAbsPath a => a -> AbsPath
toAbsPath = convert

toRelPath :: IsRelPath a => a -> RelPath
toRelPath = convert

-- === Path joining ===

class PathJoin l r path | l r -> path where
    (//) :: l -> r -> path

    default (//) :: (Convertible l path, Convertible r path, Monoid path) => l -> r -> path
    l // r = (convert l :: path) <> (convert r :: path)

infixl 9 //

instance (out~PathJoinResult a b, Convertible a out, Convertible b out, Monoid out) => PathJoin a b out

type family PathJoinResult a b where
    PathJoinResult RelPath    a          = RelPath
    PathJoinResult RelSegment a          = RelPath
    PathJoinResult a          RelPath    = RelPath
    PathJoinResult a          RelSegment = RelPath
    PathJoinResult a          b          = AbsPath

-- === instances ===

instance Show SpecialPath where
    show (SpecialPath a) = show a

instance Convertible RelPath    RelPath where safeConvert = Right
instance Convertible AbsPath    RelPath where safeConvert = Right . fmap Segment
instance Convertible String     RelPath where safeConvert = Right . fromString
instance Convertible Segment    RelPath where safeConvert = Right . toRelPath . Segment
instance Convertible RelSegment RelPath where safeConvert = Right . Path . return

instance Convertible AbsPath    AbsPath where safeConvert = Right
instance Convertible String     AbsPath where safeConvert = Right . fromString
instance Convertible Segment    AbsPath where safeConvert = Right . Path . return

instance IsString Segment    where fromString   = Node
instance IsString RelSegment where fromString   = Segment . fromString
instance IsString RelPath    where fromString s = toRelPath (fromString s :: AbsPath)
instance IsString AbsPath    where fromString   = return . Node


------------------------------------------------------------------------
-- Platform
------------------------------------------------------------------------

data Windows = Windows deriving (Show, Eq, Generic)
data Linux   = Linux   deriving (Show, Eq, Generic)
data Darwin  = Darwin  deriving (Show, Eq, Generic)

-- === utils ===

#ifdef darwin_HOST_OS
type Platform = Darwin
#endif

#ifdef linux_HOST_OS
type Platform = Linux
#endif

#ifdef mingw32_HOST_OS
type Platform = Windows
#endif

platform :: Platform
platform = def

-- === instances ===

instance Default Windows where def = Windows
instance Default Darwin  where def = Darwin
instance Default Linux   where def = Linux


------------------------------------------------------------------------
-- Repr
------------------------------------------------------------------------

class Repr a where
    repr :: a -> String

instance Repr a => Show (Path a) where
    showsPrec d (Path segs) = showParen (d > app_prec) $
            showString $ "Path \"" <> (intercalate "/" $ fmap repr segs) <> "\""
         where app_prec = 10

instance Repr Segment where
    repr = \case
        Root   -> "/"
        Node s -> s

instance Repr RelSegment where
    repr = \case
        Var     v -> "$" ++ v
        Up        -> ".."
        Segment s -> repr s
        Special s -> "@" ++ show s


------------------------------------------------------------------------
-- Resolution
------------------------------------------------------------------------

class ResolvePlatform path where
    resolveLinux   :: MonadIO m => path -> EitherT Error m RelPath
    resolveWindows :: MonadIO m => path -> EitherT Error m RelPath
    resolveDarwin  :: MonadIO m => path -> EitherT Error m RelPath

class ResolveSpecial sys where
    resolveSpecial :: MonadIO m => sys -> SpecialPath -> EitherT Error m RelPath

-- === utils ===

resolveSpecialCurrent :: MonadIO m => SpecialPath -> EitherT Error m RelPath
resolveSpecialCurrent = resolveSpecial platform

rightRelPath :: (Monad m, IsRelPath a) => a -> EitherT e m RelPath
rightRelPath = right . toRelPath

-- === instances ===

instance ResolveSpecial Linux   where resolveSpecial _ (SpecialPath p) = resolveLinux   p
instance ResolveSpecial Windows where resolveSpecial _ (SpecialPath p) = resolveWindows p
instance ResolveSpecial Darwin  where resolveSpecial _ (SpecialPath p) = resolveDarwin  p


------------------------------------------------------------------------
-- Path expansion
------------------------------------------------------------------------

class PathExpander path where
    expandPathT :: MonadIO m => path -> EitherT Error m AbsPath

expandPath :: (MonadIO m, PathExpander path) => path -> m (Either Error AbsPath)
expandPath = runEitherT . expandPathT

instance PathExpander RelPath where
    expandPathT path = do
        env <- Map.fromList <$> liftIO Env.getEnvironment
        let expandPath' = expandSegments mempty
            expandSegments path (segments -> segs) = case segs of
                []     -> right path
                (s:ss) -> case s of
                    Segment seg -> expandSegments (path // seg) ssPath
                    Var v -> do
                        envVal  <- hoistEither $ justErr (UnknownVar v) (Map.lookup v env)
                        envPath <- hoistEither $ readRelPath envVal
                        expPath <- expandPath' envPath
                        expandSegments (expPath // path) ssPath
                    Up -> case path of
                        Path [] -> left NegativePath
                        Path p  -> expandSegments (Path $ init p) ssPath
                    Special s -> do
                        p   <- resolveSpecialCurrent s
                        pex <- expandPathT p
                        expandSegments (path // pex) ssPath
                    where ssPath = Path ss
        expandPath' $ path

instance PathExpander AbsPath where
    expandPathT = return


------------------------------------------------------------------------
-- Specials
------------------------------------------------------------------------

-- === Temp ===

data Temp = Temp Locality Persistence
             deriving (Show, Typeable)

data Locality = Global
              | Local
              deriving (Show, Generic)

data Persistence = Persistent
                 | NonPersistent
                 deriving (Show, Typeable)

instance ResolvePlatform Temp where
    resolveLinux = \case
        Temp Global NonPersistent -> rightRelPath $ root // "tmp"
        Temp Global    Persistent -> rightRelPath $ root // "var" // "tmp"
        Temp Local  NonPersistent -> rightRelPath $ var "TMPDIR"
        Temp Local     Persistent -> rightRelPath $ home // ".tmp"

    resolveDarwin = \case
        Temp Global NonPersistent -> rightRelPath $ root // "tmp"
        Temp Global    Persistent -> rightRelPath $ root // "var" // "tmp"
        Temp Local  NonPersistent -> rightRelPath $ var "TMPDIR"
        Temp Local     Persistent -> rightRelPath $ home // ".tmp"

    resolveWindows = error "TODO"


-- === Home ===

data Home = Home deriving (Show, Typeable)

instance ResolvePlatform Home where
    resolveLinux   _ = right $ var "HOME"
    resolveDarwin  _ = right $ var "HOME"
    resolveWindows _ = right $ var "HOME"


------------------------------------------------------------------------
-- Primitives
------------------------------------------------------------------------

root :: AbsPath
root = return Root

node :: String -> AbsPath
node = return . Node

home :: RelPath
home = specialPath Home

var :: String -> RelPath
var = return . Var

up :: RelPath
up = return Up

tmp :: Locality -> Persistence -> RelPath
tmp = specialPath .: Temp

tmpDir :: RelPath
tmpDir = tmp Local NonPersistent











data Error = UnknownSpecial String
           | UnknownVar     String
           | NegativePath
           deriving (Show)



--withRightM2 :: Monad t =>    m a ->  (a ->    m b)  ->   (m b)
--                       -> t (m a) -> (a -> t (m b)) -> t (m b)




----m (t (m b))

nonEmpty = filter (/= mempty)

readRelPath :: String -> Either Error RelPath
readRelPath str@(s:_)  = fmap (Path . mount . concat) . sequence $ fmap readSeg segs where
    mount     = if s == '/' then (Segment Root:) else id
    segs      = nonEmpty $ splitOn "/" str
    readSeg s = if and $ fmap (=='.') s
        then Right $ replicate (length s - 1) Up
        else fmap return $ case s of
            --"~"    -> Right home
            (a:as) -> case a of
                '$' -> Right $ Var as
                --'@' -> readSpecial as
                _   -> Right . Segment $ Node s
    --readSpecial s = case s of
    --    "Home"         -> Right $ home
    --    --"AppData"      -> Right $ appData
    --    "LocalAppData" -> Right $ localAppData
    --    "Temp"         -> Right $ temp
    --    "Documents"    -> Right $ documents
    --    _              -> Left $ UnknownSpecial s















main = do
    let p1 = var "HOME" // "a" // "b"
        p2 = home // "a"
        p3 = tmpDir
    print p1
    print p2
    print p3
    print "---"
    print =<< expandPath p1
    print =<< expandPath p2
    print =<< expandPath p3
    print "end"
    print $ 2/3
    print $ readRelPath "/home/$foo/@ha/bar"




--special :: (PathClass Platform path, Show path, Binary path) => path -> RelSegment
--special = Special . SpecialPath

--instance Show SpecialPath where
--    show (SpecialPath p) = show p






        --data Library (scope :: Scope) = Library deriving (Show, Eq, Generic, Typeable)

---- === Persistence ===
--data Persistence = Persistent
--                 | Temporary
--                 deriving (Show, Typeable)


--temporary :: Proxy Persistent
--temporary = Proxy

--persistent :: Proxy Persistent
--persistent = Proxy


--deriving instance Typeable Persistent
--deriving instance Typeable Temporary

---- === Scope ===
--data Scope = Global
--           | Local
--           | System
--           | Network
--           deriving (Show, Eq, Generic, Typeable)

--global :: Proxy Global
--global = Proxy

--local :: Proxy Local
--local = Proxy

--system :: Proxy System
--system = Proxy

--network :: Proxy Network
--network = Proxy

--deriving instance Typeable Global
--deriving instance Typeable Local
--deriving instance Typeable System
--deriving instance Typeable Networ

--instance (Typeable tp) => Show (Temp tp) where
--    show _ = "Temp " ++ showProxyType (Proxy :: Proxy tp)



--showProxyType = drop 6 . show . typeOf

--instance PathClass Darwin (Temp User) where
--    expandPath _ _ = return [Root, Node "var", Node "xxx"]

--instance PathClass Darwin VarTemp where
--    expandPath _ _ = return [Root, Node "var", Node "tmp"]

--instance PathClass Darwin GlobalTemp where
--    expandPath _ _ = return [Root, Node "tmp"]


   --    expandSpecial sys = \case
    --        Home -> do
    --            homeDir <- Directory.getHomeDirectory
    --            let homePath = readRelPath homeDir
    --            case homePath of
    --                Left  e -> return $ Left e
    --                Right p -> expandPath p
    --        Users          -> return $ Right [Root, Node "Users"]
    --        LocalLibrary   -> fmap (++[Node "Library"]) <$> expandSpecial sys Home
    --        GlobalLibrary  -> return $ Right [Root, Node "Library"]
    --        SystemLibrary  -> return $ Right [Root, Node "System", Node "Library"]
    --        NetworkLibrary -> return $ Right [Root, Node "Network", Node "Library"]

--class PathClass sys path where
--    expandPath :: MonadIO m => sys -> path -> m Path

        --data Windows = Windows deriving (Show, Eq, Generic)
        --data Linux   = Linux   deriving (Show, Eq, Generic)
        --data Darwin  = Darwin  deriving (Show, Eq, Generic)






        --class PathClass path where
        --    expandLinux   :: MonadIO m => path -> m Path
        --    expandWindows :: MonadIO m => path -> m Path
        --    expandDarwin  :: MonadIO m => path -> m Path

        --instance PathClass (Temp Global Persistent) where
        --    expandDarwin _ = return $ [Root, Node "var", Node "temp"]

        --main = do
        --    let path = temp global persistent -- // "foo"
        --    --print path
        --    print "end"


        --platform = getPlatform (Proxy :: Proxy HOST_OS)

        --class GetPlatform (name :: Symbol) p | name -> p where
        --    getPlatform :: Proxy name -> p

        --instance GetPlatform "darwin"  Darwin  where getPlatform _ = Darwin
        --instance GetPlatform "windows" Windows where getPlatform _ = Windows
        --instance GetPlatform "linux"   Linux   where getPlatform _ = Linux


--class SpecialPath sys where
--    expandSpecial :: sys -> Special -> IO (Either Error Path)

    --instance SpecialPath Linux where
    --    expandSpecial _ = \case
    --        Home -> do
    --            homeDir <- Directory.getHomeDirectory
    --            let homePath = readRelPath homeDir
    --            case homePath of
    --                Left  e -> return $ Left e
    --                Right p -> expandPath p
    --        Users -> return $ Right [Root, Node "home"]

    --instance SpecialPath Darwin where
    --    expandSpecial sys = \case
    --        Home -> do
    --            homeDir <- Directory.getHomeDirectory
    --            let homePath = readRelPath homeDir
    --            case homePath of
    --                Left  e -> return $ Left e
    --                Right p -> expandPath p
    --        Users          -> return $ Right [Root, Node "Users"]
    --        LocalLibrary   -> fmap (++[Node "Library"]) <$> expandSpecial sys Home
    --        GlobalLibrary  -> return $ Right [Root, Node "Library"]
    --        SystemLibrary  -> return $ Right [Root, Node "System", Node "Library"]
    --        NetworkLibrary -> return $ Right [Root, Node "Network", Node "Library"]


    --home           = Special Home
    --localLibrary   = Special LocalLibrary
    --globalLibrary  = Special GlobalLibrary
    --systemLibrary  = Special SystemLibrary
    --networkLibrary = Special NetworkLibrary
    --temp           = Special Temp
    --documents      = Special Documents

    ----instance FromText RelPath where
    ----    fromText = fromString . convert

    ----instance IsString RelPath where
    ----    fromString = readRelPath

    --data Error = UnknownSpecial String
    --           | UnknownVar     String
    --           | NegativePath
    --           deriving (Show)

    --nonEmpty = filter (/= mempty)

    --readRelPath :: String -> Either Error RelPath
    --readRelPath str@(s:_)  = fmap (mount . concat) . sequence $ fmap readSeg segs where
    --    mount     = if s == '/' then (Segment Root:) else id
    --    segs      = nonEmpty $ splitOn "/" str
    --    readSeg s = if and $ fmap (=='.') s
    --        then Right $ replicate (length s - 1) Up
    --        else fmap return $ case s of
    --            "~"    -> Right home
    --            (a:as) -> case a of
    --                '$' -> Right $ Var as
    --                '@' -> readSpecial as
    --                _   -> Right . Segment $ Node s
    --    readSpecial s = case s of
    --        "Home"         -> Right $ home
    --        --"AppData"      -> Right $ appData
    --        "LocalAppData" -> Right $ localAppData
    --        "Temp"         -> Right $ temp
    --        "Documents"    -> Right $ documents
    --        _              -> Left $ UnknownSpecial s


    ----readPath :: String -> Either String Path
    ----readPath = join . fmap sequence . (fmap . fmap) checkSegment . readRelPath where
    ----    checkSegment = \case
    ----        Segment s -> Right s
    ----        _         -> Left "ohno"

    --expandPath :: RelPath -> IO (Either Error Path)
    --expandPath path = do
    --    env <- Map.fromList <$> Env.getEnvironment
    --    let expandPath' = expandSegments []
    --        expandSegments path segs = case segs of
    --            []     -> return . Right $ path
    --            (s:ss) -> case s of
    --                Segment s -> expandSegments (s:path) ss
    --                Var     v -> withRight (justErr (UnknownVar v) $ Map.lookup v env)
    --                          $ \sp      -> withRight  (readRelPath sp)
    --                          $ \envPath -> withRightM (expandPath' envPath)
    --                          $ \p       -> expandSegments (p ++ path) ss
    --                Up        -> case path of
    --                    []     -> return $ Left NegativePath
    --                    (p:ps) -> expandSegments ps ss
    --                Special s -> withRightM (expandSpecial platform s)
    --                           $ \p -> expandSegments (reverse p ++ path) ss
    --    (fmap.fmap) reverse . expandPath' $ path


    --withRight val f = case val of
    --    Left  e -> return $ Left e
    --    Right r -> f r

    --withRightM mval f = do
    --    val <- mval
    --    withRight val f

    --main = do
    --    let p = readRelPath "@Home/.."
    --    print p
    --    case p of
    --        Left  _ -> return ()
    --        Right r -> do
    --            p <- expandPath r
    --            print p
    --    print $ HOST_OS
    --    print $ platform

    --platform = getPlatform (Proxy :: Proxy HOST_OS)

    --class GetPlatform (name :: Symbol) p | name -> p where
    --    getPlatform :: Proxy name -> p

    --instance GetPlatform "darwin"  Darwin  where getPlatform _ = Darwin
    --instance GetPlatform "windows" Windows where getPlatform _ = Windows
    --instance GetPlatform "linux"   Linux   where getPlatform _ = Linux

--instance ToText Segment where
--    toText = \case
--        Root   -> "/"
--        Node t -> t

--instance ToText RelSegment where
--    toText = \case
--        Var t  -> "$" <> t
--        Home   -> "~"
--        Up     -> ".."
--        Segment s -> toText s

--instance ToText RelPath where
--    toText (p:ps) = if p == Segment Root then "/" <> align ps
--                                         else align $ p:ps
--        where align = Text.intercalate "/" . fmap toText


--instance IsString RelPath where
--    fromString = fromText . convert

--main = do
--    --let path = ("/home/wdanilo/$foo/bar" :: RelPath)
--    --print $ path
--    --print $ toText path
--    print "end"


--toUnixString :: UniPath -> FilePath
--toUnixString []   = ""
--toUnixString path = case head l of
--        "/" -> "/" ++ join (tail l)
--        _   -> join l
--    where l    = toList path
--          join = StringUtils.join "/"


--toUnixString' :: MonadIO m => UniPath -> m FilePath
--toUnixString' = expand >=> return . toUnixString


--expand :: MonadIO m => UniPath -> m UniPath
--expand [] = return def
--expand (x:xs) = liftIO $ case x of
--    Var "~"              -> expandRest   Directory.getHomeDirectory
--    Var "$APPDATA"       -> expandRest   Directory.getAppDataDirectory
--    Var "$LOCALAPPDATA"  -> expandRest   Directory.getLocalAppDataDirectory
--    Var "$TEMP"          -> expandRest   Directory.getTemporaryDirectory
--    Var "$DOCUMENTS"     -> expandRest   Directory.getUserDocumentsDirectory
--    Var ('$':name)       -> expandRest $ Env.getEnv name
--    _       -> (:) x <$> expand xs
--    where expandRest fvar = do
--              var <- fvar
--              rest <- expand xs
--              return $ fromUnixString var ++ rest


--fromList :: [String] -> UniPath
--fromList = foldr prepend def


--toList :: UniPath -> [String]
--toList = fmap str where
--    str item = case item of
--            Node txt -> txt
--            Root txt -> txt
--            Up       -> ".."
--            Current  -> "."
--            Empty    -> ""
--            Var v    -> v



--fromFilePath :: FilePath.FilePath -> UniPath
--fromFilePath = fromUnixString    -- TODO[PMocz] does that really work?


--toFilePath :: UniPath -> FilePath.FilePath
--toFilePath = FilePath.joinPath . toList


---- FIXME[wd]: poprawic caly UniPath !!! kawalek refactoru:
----instance IsList UniPath where
----    type (Item UniPath) = String
----    fromList  = foldr prepend def
----    toList    = fmap str where
----        str item = case item of
----                Node txt -> txt
----                Root txt -> txt
----                Up       -> ".."
----                Current  -> "."
----                Empty    -> ""
----                Var v    -> v

--append :: String -> UniPath -> UniPath
--append snode path = path ++ [toPathItem snode]


--prepend :: String -> UniPath -> UniPath
--prepend snode path = toPathItem snode : path


--dirOf :: UniPath -> UniPath
--dirOf = init


--toPathItem :: String -> PathItem
--toPathItem snode = case snode of
--        "/"  -> Root "/"
--        ".." -> Up
--        "."  -> Current
--        ""   -> Empty
--        txt  -> Node txt


--normalise :: UniPath -> UniPath
--normalise path = case reverse (normaliseR (reverse path) 0) of
--        [] -> [Current]
--        p  -> p


--normaliseR :: UniPath -> Int -> UniPath
--normaliseR path undo = case path of
--        []   -> replicate undo Up
--        x:xs -> case x of
--                   root@(Root _) -> [root]
--                   Up            -> normaliseR xs (undo+1)
--                   Current       -> if null xs && undo == 0
--                                        then path
--                                        else normaliseR xs undo
--                   Empty         -> normaliseR xs undo
--                   _             -> if undo > 0
--                                        then normaliseR xs (undo-1)
--                                        else x:normaliseR xs undo


--fileName :: UniPath -> FilePath
--fileName path = case last $ normalise path of
--                  Node fname -> List.intercalate "." $ StringUtils.split "." fname
--                  _          -> error "something is wrong with the path " ++ toUnixString path

----removes the name of a file, if present
--basePath :: UniPath -> UniPath
--basePath path = normalise $ case last $ normalise path of
--                              Node _ -> path ++ [Up]
--                              _      -> path


--extension :: UniPath -> FilePath
--extension = FilePath.takeExtension . toUnixString


--addExtension :: String -> UniPath -> UniPath
--addExtension ext path =
--    normalise $ path ++ [Up] ++ [Node $ fileName path ++ ext]


--replaceExtension :: String -> UniPath -> UniPath
--replaceExtension ext = fromUnixString . FilePath.replaceExtension ext . toUnixString


--dropExtension :: UniPath -> UniPath
--dropExtension = fromUnixString . FilePath.dropExtension . toUnixString


--makeRelative :: UniPath -> UniPath -> UniPath
--makeRelative path1 =
--    fromUnixString . FilePath.makeRelative (toUnixString path1) . toUnixString
