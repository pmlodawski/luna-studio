
{-# LANGUAGE NoImplicitPrelude #-}

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
{-# LANGUAGE ConstraintKinds            #-}

{-# LANGUAGE NoOverloadedStrings            #-}
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE BangPatterns            #-}

{-# LANGUAGE CPP #-}



module System.Path where

#if __GLASGOW_HASKELL__ >= 710

--import           Control.Monad      ((>=>))
import           Data.Binary        (Binary)
import           Data.Typeable
import           Data.List.Split    (splitOn)
--import qualified Data.List          as List
--import qualified Data.T.Utils  as TextUtils
import qualified System.Directory   as Directory
import qualified System.Environment as Environment
import           Control.Exception  (catch, SomeException)
import           Control.Monad      (join)
--import           System.IO.Error    (IOError, isDoesNotExistError)
import           System.IO.Error    (tryIOError)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Control.Error.Safe (justErr)
import           Control.Error.Util
import           Data.Binary        hiding (get, put)
import           Text.Read          (readMaybe)
--import           Flowbox.Prelude                    hiding (empty, fromList, toList)
--import qualified Flowbox.System.Directory.Locations as Directory
import           System.IO.Unsafe (unsafePerformIO)
import           Data.IORef
import           Unsafe.Coerce (unsafeCoerce)
import           Flowbox.Prelude hiding (expand, lookup, splitAt, repr)
import           GHC.TypeLits

import           Control.Monad.Shuffle (deepBind, (>>>=), ($>>=))
import           Data.Foldable (fold)
import           Data.Traversable (sequenceA)
import           Data.Maybe (fromMaybe)
--import           Data.ConcreteTypeRep

import qualified Data.Map as Map
import           Control.Monad.Trans.Either
import           Data.List (intercalate)
import           System.Directory as Dir
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Internal.Lazy.Fusion as TF
import qualified Data.Text.Internal.Fusion.Common as TF
import qualified Data.Text.Internal.Fusion.Types as TF

import qualified System.Posix.Files as Posix
import qualified System.Posix.Types as Posix
import qualified Foreign.C.Types as C

import           GHC.Int (Int64, Int32)

import qualified Control.Monad.State          as State
import           Control.Monad.State.Generate (newState)
import           Control.Monad (foldM)
import           Control.Monad ((>=>))

import           System.Platform



-----------------------------------------------------------------------





maybeToEither  = flip maybe Right . Left

maybeToEitherT :: Monad m => e -> Maybe a -> EitherT e m a
maybeToEitherT = hoistEither .: maybeToEither



head' :: Text -> Maybe Char
head' (TF.stream -> TF.Stream next s _len) = headLoop s where
    headLoop !s = case next s of
        TF.Yield x _ -> Just x
        TF.Skip s'   -> headLoop s'
        TF.Done      -> Nothing

withLeft :: (e -> f) -> Either e a -> Either f a
withLeft f = \case
        Left  e -> Left $ f e
        Right a -> Right a

joinEither :: Either a a -> a
joinEither = \case
    Left  a -> a
    Right a -> a

joinEitherT :: Functor m => EitherT a m a -> m a
joinEitherT = fmap joinEither . runEitherT


data Uni  = Uni  deriving (Show, Eq, Generic)

withLeftSquash :: Monad m => (e -> f) -> EitherT f m (Either e b) -> EitherT f m b
withLeftSquash f mv = do
    v <- mv
    hoistEither $ withLeft f v

------------------------------------------------------------------------
-- Route
------------------------------------------------------------------------

newtype Env = Env (Map Text Text) deriving (Show, Read, Typeable, Generic, Monoid)

$(newState "EnvState" ''Env)

instance MonadEnvState IO where
    get           = Env . Map.fromList . fmap (over both fromString) <$> Environment.getEnvironment
    put (Env map) = mapM_ setEnvTuple els where
        els = Map.assocs map
        setEnvTuple (k,v) = Environment.setEnv (convert k) (convert v)


type MonadEnvIO m = (MonadEnvState m, MonadIO m)


lookup :: MonadEnvState m => Text -> m (Either Error Text)
lookup = runEitherT . lookupT

lookupT :: MonadEnvState m => Text -> EitherT Error m Text
lookupT v = do
    Env env <- get
    Map.lookup v env & justErr (UnknownVar v)
                     & hoistEither

--data Node seg = Node (Maybe (NodeType seg)) (Path seg)
--              deriving (Show, Read, Typeable, Generic)

--newtype FsSpec seg = FsSpec [Node seg]
--                   deriving (Show, Read, Typeable, Generic)

--data NodeType seg = File
--                  | Folder
--                  | Link (Path seg)
--                  deriving (Show, Read, Typeable, Generic)

data LinkType = Soft
              | Hard
              deriving (Show, Read, Typeable, Generic)



data Mount = Root
           | Network
           deriving (Show, Read, Typeable, Generic, Eq)

--data Special = AppData Locality
--             | Temp Locality Persistence
--             | Home
--             | Documents
--             | Desktop
--             | Downloads
--             | Movies
--             | Music
--             | Pictures
--             deriving (Show, Read, Typeable, Generic)

data Path = Path { _mountPoint :: Maybe Mount
                 , _segments   :: [Node]
                 } deriving (Show, Read, Typeable, Generic, Eq)


type IsPath p = Convertible p Path


data Node = Var  Text
          | Rel  Int64
          | Node Text
          deriving (Show, Read, Typeable, Generic, Eq)


var     = Var . convert
segment = Node . convert

---
data Error = UnknownVar     Text
           | UnknownSegment Text
           | NegativePath
           | MalformedPath
           | UnexistingPath Path
           | FileAccessError Path
           | EmptyPath
           | NoPathFound
           deriving (Show, Read, Typeable, Generic, Eq)
---

makeLenses ''Path

instance Monoid Path where
    mempty                            = Path Nothing mempty
    (Path m s) `mappend` (Path m' s') = case m' of
        Just _  -> Path m' s'
        Nothing -> Path m (s <> s')




instance FromText Path where
    fromText = readPath

instance IsString Path where
    fromString = fromText . convert


-- Temp

data Locality = Global
              | Local
              deriving (Show, Read, Typeable, Generic)

data Persistence = Persistent
                 | NonPersistent
                 deriving (Show, Read, Typeable, Generic)


-----------------------------------------------------------------------

--"///home/"

countPrefix c = T.length . fst . T.break (/= c)

isMonoText txt = case head' txt of
    Nothing -> False
    Just h  -> countPrefix h txt == T.length txt

isMonoTextOf c txt = case head' txt of
    Nothing -> False
    Just h  -> if c == h then countPrefix h txt == T.length txt
                         else False

nonEmpty = filter (/= mempty)


splitHead = \case
    []     -> Nothing
    (a:as) -> Just (a,as)

type Reader a = Text -> Maybe a

leftIf :: Monad m => e -> Bool -> EitherT e m ()
leftIf e cond = if cond then left e
                        else return ()

-- === Native Path reading ===

class NativeToUni os where
    nativeToUni :: Convertible a Text => os -> a -> Text

instance NativeToUni Darwin  where nativeToUni _ = convert
instance NativeToUni Linux   where nativeToUni _ = convert
instance NativeToUni GHCJS   where nativeToUni _ = convert
instance NativeToUni Windows where
    nativeToUni _ (T.unpack . convert -> s) = convert varPath where
        slashPath = replace '\\' '/' s
        drivePath = if length slashPath < 2 then slashPath
                                            else case slashPath of
                                                (disk : ':' : '/' : ss) -> '/' : disk : '/' : ss
                                                _                       -> slashPath
        varPath = varConv drivePath
        varConv = \case
            ('%' : ss) -> '$' : varConv' ss
            (s   : ss) -> s   : varConv  ss
            []         -> []
        varConv' = \case
            ('%' : ss) -> varConv  ss
            (s   : ss) -> s : varConv' ss
            []         -> []
        replace c c' = \case
            (s : ss) -> if c == s then c' : rest
                                  else s  : rest
                        where rest = replace c c' ss
            []       -> []

readNative :: Convertible a Text => a -> Path
readNative = readNativeFor platform

readNativeFor :: (Convertible a Text, NativeToUni os) => os -> a -> Path
readNativeFor = readPath .: nativeToUni


-- === Unified Path reading ===

readPath :: Convertible a Text => a -> Path
readPath (convert -> txt)  = Path mount segs where
    segs         = readSegs Node segReaders strSegs
    headSeg      = fmap fst $ splitHead strSegs
    strSegs      = nonEmpty $ T.splitOn (convert "/") txt
    segReaders   = [varReader, relReader] -- homeReader

    --homeReader :: Text -> Maybe Node
    --homeReader s = if s == (convert "~") then Just $ var "$HOME"
    --                                     else Nothing

    mount :: Maybe Mount
    mount = case countPrefix '/' txt of
          0 -> Nothing
          2 -> Just Network
          _ -> Just Root

    varReader :: Text -> Maybe Node
    varReader s = maybeStartsWith '$' s var

    relReader :: Text -> Maybe Node
    relReader txt = if isMonoTextOf '.' txt then Just $ Rel (T.length txt - 1)
                                            else Nothing

    ---

    readSegs :: (Text -> a) -> [Text -> Maybe a] -> [Text] -> [a]
    readSegs fdef readers segs = fmap (\s -> fromMaybe (fdef s) $ readSeg readers s) segs

    readSeg :: [Text -> Maybe a] -> Text -> Maybe a
    readSeg readers seg = case readers of
        []     -> Nothing
        (r:rs) -> case r seg of
            Nothing -> readSeg rs seg
            Just a  -> Just a

    maybeStartsWith c txt f = case head' txt of
        Just h -> if c == h then Just $ f (T.tail txt)
                            else Nothing
        _      -> Nothing


-- === Environment expand ===

expandVars' :: (MonadEnvState m, PathLike Error m p) => p -> m (Either Error Path)
expandVars' = runEitherT . expandVars


liftPath :: forall m p. (Monad m, PathJoin2 (m Path) p) => p -> m Path
liftPath p = (return mempty :: m Path) // p


expandVars :: (MonadEnvState m, PathLike Error m p) => p -> EitherT Error m Path
expandVars p = do
    Path m segs <- liftPath p
    fmap (foldl (<>) (Path m [])) $ mapM expandSegment segs
    where expandSegment = \case
              Var v -> (readNative <$> lookupT v) >>= expandVars
              s     -> return $ single s

--

expandHome' :: (MonadEnvState m, PathLike Error m p) => p -> m (Either Error Path)
expandHome' = runEitherT . expandHome

expandHome :: (MonadEnvState m, PathLike Error m p) => p -> EitherT Error m Path
expandHome p = do
    (Path m segs) <- liftPath p
    homeEnv <- lookupT homeVar
    return . foldl (<>) (Path m []) $ fmap (expandSegment homeEnv) segs
    where homeVar       = convert "HOME"
          expandSegment homeEnv = \case
                Node s -> if s == convert "~" then readNative homeEnv
                                              else single (Node s)
                a      -> single a

--

expandEnv' :: (MonadEnvState m, PathLike Error m p) => p -> m (Either Error Path)
expandEnv' = runEitherT . expandEnv

expandEnv :: (MonadEnvState m, PathLike Error m p) => p -> EitherT Error m Path
expandEnv = expandVars >=> expandHome


-- === normalize ===

normalize :: MonadEnvState m => Path -> m (Either Error Path)
normalize = runEitherT . normalizeT

normalizeT :: MonadEnvState m => Path -> EitherT Error m Path
normalizeT p = expandEnv p >>= normalizeUnsafe

-- | Normalization without variable expansion can lead to incorrect paths
normalizeUnsafe :: Monad m => Path -> EitherT Error m Path
normalizeUnsafe = hoistEither . normalizeUnsafe'

normalizeUnsafe' :: Path -> Either Error Path
normalizeUnsafe' path@(Path m segs) = fmap (Path m . reverse)
                                   $ normSegments [] segs
    where normSegments expSegs segs = case segs of
              []     -> Right expSegs
              (s:ss) -> case s of
                  Rel i       -> do
                      newSegs <- expandRel s expSegs
                      normSegments newSegs ss
                  _           -> normSegments (s : expSegs) ss

          expandRel (Rel 0) expSegs = Right expSegs
          expandRel (Rel i) expSegs = case expSegs of
              []      -> case m of
                         Nothing      -> Right [Rel i]
                         _            -> Left  NegativePath
              [Rel j] -> Right [Rel $ i + j]
              (e:es)  -> expandRel (Rel $ i - 1) es




-- === cwd ===

cwd :: MonadIO m => m Path
cwd = liftIO $ readNative <$> Dir.getCurrentDirectory


-- === expand ===

expand' :: PathLikeEnvIO Error m p => p -> m (Either Error Path)
expand' = runEitherT . expand

expand :: PathLikeEnvIO Error m p => p -> EitherT Error m Path
expand path = normalizeT =<< cwd // path


split :: Path -> Maybe (Path, Node)
split (Path m segs) = if null segs then Nothing
                                   else Just (Path m $ init segs, last segs)


base :: Path -> Maybe Path
base = fmap fst . split

lastSegment :: Path -> Maybe Node
lastSegment = fmap snd . split

commonPrefix :: [Path] -> Maybe Path
commonPrefix = \case
    []     -> Nothing
    (s:[]) -> Just s
    (s:ss) -> foldM commonPathSegs s ss
    where commonPathSegs (Path m segs) (Path m' segs') =
              if (m == m') then Just $ Path m $ commonSegs segs segs'
                           else Nothing
          commonSegs (s:ss) (s':ss') = if s == s' then s : commonSegs ss ss'
                                                  else []
          commonSegs _      _        = []


class OsRepr os where
    osRepr :: os -> Path -> Text

instance OsRepr Uni where
    osRepr _ (Path m segs) = mountRepr m <> segRepr where
        segRepr   = T.intercalate (convert "/") $ concat $ fmap showSeg segs
        showSeg   = \case
            Var  v -> [T.cons '$' v]
            Rel  i -> replicate (i - 1) (convert "..")
            Node t -> [t]
        mountRepr = \case
            Just Root    -> convert "/"
            Just Network -> convert "//"
            Nothing      -> convert ""
        replicate i a = case i of
            0 -> []
            _ -> a : replicate (i-1) a

instance OsRepr Windows where
    osRepr _ (Path m segs) = mountRepr m <> segRepr where
        (dSeg, pSegs) = case segs of
            (Node s : ss) -> (Just s , ss  )
            _             -> (Nothing, segs)
        segRepr   = T.intercalate (convert "\\") $ concat $ fmap showSeg pSegs
        showSeg   = \case
            Var  v -> [T.snoc (T.cons '%' v) '%']
            Rel  i -> replicate (i - 1) (convert "..")
            Node t -> [t]
        mountRepr = \case
            Just Root    -> case dSeg of
                Just s  -> T.snoc s ':'
                Nothing -> convert ""
            Just Network -> convert "\\"
            Nothing      -> convert ""
        replicate i a = case i of
            0 -> []
            _ -> a : replicate (i-1) a

instance OsRepr Darwin where osRepr _ = osRepr Uni
instance OsRepr Linux  where osRepr _ = osRepr Uni
instance OsRepr GHCJS  where osRepr _ = osRepr Uni


native :: MapPath p
       => p -> MapPathOut p Text
native = nativeFor platform


nativeFor :: (MapPath p, OsRepr os)
          => os -> p -> MapPathOut p Text
nativeFor = mapPath . osRepr

repr :: MapPath p
     => p -> MapPathOut p Text
repr = nativeFor Uni



---------------------------------- TODO TODO TODO

class MapPathM m p where
    mapPathM :: (Path -> m t) -> p -> m t

--type family MapPathOutM p m t where
--    MapPathOutM (n Path) m t = m t
--    MapPathOutM Path     m t = m t


instance MapPathM m Path where
    mapPathM  = ($)

instance (m ~ n, Monad m) => MapPathM m (n Path) where
    mapPathM f mp = f =<< mp



class MapPath p where
    mapPath  :: (Path -> t) -> p -> MapPathOut p t

type family MapPathOut p t where
    MapPathOut (m Path) t = m t
    MapPathOut a        t = t

instance MapPath Path where
    mapPath = ($)

instance MapPath String where
    mapPath f = f . convert

instance Functor m => MapPath (m Path) where
    mapPath = fmap . mapPath



class MapPath2 t p1 p2 where
    mapPath2  :: (Path -> Path -> t) -> p1 -> p2 -> MapPathOut2 p1 p2 t

type family MapPathOut2 p1 p2 t where
    MapPathOut2 (m Path) p        t = m t
    MapPathOut2 p        (m Path) t = m t
    MapPathOut2 a        p        t = t

instance MapPath2 t Path Path where
    mapPath2 f a b = f a b

instance (Convertible a Path, Convertible b Path, t ~ MapPathOut2 a b t)
      => MapPath2 t a b where mapPath2 f a b = f (convert a) (convert b)

instance (m ~ n, Monad m) => MapPath2 t (m Path) (n Path) where
    mapPath2 f mp mp' = do
        p  <- mp
        p' <- mp'
        return $ f p p'

instance (Monad m, Convertible a Path, MapPathOut2 a (m Path) t ~ m t)
      => MapPath2 t a (m Path) where
    mapPath2 f a mp = do
        p  <- mp
        return $ f (convert a) p

instance (Monad m, Convertible a Path)
      => MapPath2 t (m Path) a where
    mapPath2 f mp a = do
        p  <- mp
        return $ f p (convert a)



class MapPath2x a b p1 p2 out where
    mapPath2x  :: (a -> b -> out) -> p1 -> p2 -> MapPathOut2x a b p1 p2 out

type family MapPathOut2x a b p1 p2 (t :: *) where
    MapPathOut2x a b a     b     t = t
    MapPathOut2x a b (m a) (m b) t = m t
    MapPathOut2x a b (m a) p     t = m t
    MapPathOut2x a b p     (m b) t = m t
    MapPathOut2x a b p     r     t = t

instance MapPath2x a b a b out where
    mapPath2x f a b = f a b

--instance (Convertible a Path, Convertible b Path, t ~ MapPathOut2x a b t)
--      => MapPath2x t a b where mapPath2x f a b = f (convert a) (convert b)

--instance (m ~ n, Monad m) => MapPath2x t (m Path) (n Path) where
--    mapPath2x f mp mp' = do
--        p  <- mp
--        p' <- mp'
--        return $ f p p'

--instance (Monad m, Convertible a Path, MapPathOut2x a (m Path) t ~ m t)
--      => MapPath2x t a (m Path) where
--    mapPath2x f a mp = do
--        p  <- mp
--        return $ f (convert a) p

--instance (Monad m, Convertible a Path)
--      => MapPath2x t (m Path) a where
--    mapPath2x f mp a = do
--        p  <- mp
--        return $ f p (convert a)



class MapPathM2 m p1 p2 where
    mapPathM2  :: (Path -> Path -> m t) -> p1 -> p2 -> m t

instance MapPathM2 m Path Path where
    mapPathM2 f a b = f a b

instance MapPathM2 m String String where mapPathM2 f a b = f (convert a) (convert b)
instance MapPathM2 m String Text   where mapPathM2 f a b = f (convert a) (convert b)
instance MapPathM2 m Text   String where mapPathM2 f a b = f (convert a) (convert b)
instance MapPathM2 m Text   Text   where mapPathM2 f a b = f (convert a) (convert b)

instance (m ~ n, m ~ n', Monad m) => MapPathM2 m (n Path) (n' Path) where
    mapPathM2 f mp mp' = do
        p  <- mp
        p' <- mp'
        f p p'

--mp2 :: (Path -> Path -> t)

--mp2 :: (MapPath p1, MapPath p, MapPathOut p1 t1 ~ (Path -> t)) =>
--     (Path -> t1) -> p1 -> p -> MapPathOut p t

--mp2 ::
--     (Path -> Path -> t) -> p1 -> p2 -> out
--mp2 f p p' = mapPath (mapPath f p) p'


---------------------------------- TODO TODO TODO


--instance (Functor m, XRepr a) => XRepr (m a) where
--    xrepr = fmap xrepr



--osPath :: Path -> Text

data Existence = OK
               | Missing
               | Broken
               | WrongPath Error
               deriving (Show, Read, Typeable, Generic, Eq)



fileStatus' :: PathLikeEnvIO Error m p => p -> m (Either Error Posix.FileStatus)
fileStatus' = runEitherT . fileStatus

fileStatus :: PathLikeEnvIO Error m p => p -> EitherT Error m Posix.FileStatus
fileStatus p = do
    expPath      <- expand p
    (ppath,node) <- split expPath
                  & maybeToEitherT EmptyPath
    let ppathNat  = convert $ native ppath
        natPath   = convert $ native expPath

    dirExist <- liftIO $ Directory.doesDirectoryExist ppathNat
    UnexistingPath ppath `leftIf` not dirExist

    lst <- liftIO $ fmap convert <$> Directory.getDirectoryContents ppathNat
    UnexistingPath expPath `leftIf` not (native (single node) `elem` lst)

    withLeftSquash (const $ FileAccessError expPath) $ liftIO . tryIOError
                                                     $ Posix.getFileStatus natPath


check :: (PathLikeEnvIO Error (EitherT Existence m) p, Functor m, Monad m)
      => p -> m Existence
check p = joinEitherT $ do
    expPath      <- withLeftSquash WrongPath     $ runEitherT $ expand p
    fstat        <- withLeftSquash convertErrors $ runEitherT $ fileStatus p
    let natPath   = convert $ native expPath

    okIf . not $ Posix.isSymbolicLink fstat
    link <- liftIO $ readNative <$> Posix.readSymbolicLink natPath
    okIf $ link == expPath

    return Broken

    where missingIf     = leftIf Missing
          okIf          = leftIf OK
          convertErrors = \case
              EmptyPath        -> OK
              UnexistingPath _ -> Missing
              _                -> Broken





exists :: (PathLikeEnvIO Error (EitherT Existence m) p, Functor m, Monad m)
       => p -> m Bool
exists p = existBool <$> check p where
    existBool = \case
        OK     -> True
        Broken -> True
        _      -> False



type PathLike      e m p = PathJoin2 (EitherT e m Path) p
type PathLikeEnvIO e m p = (PathLike e m p, MonadEnvIO m)
type PathLikeEnv   e m p = (PathLike e m p, MonadEnvState m)


size' :: PathLikeEnvIO Error m p => p -> m (Either Error Int64)
size' = runEitherT . size

size :: PathLikeEnvIO Error m p => p -> EitherT Error m Int64
size p = unpack . Posix.fileSize <$> fileStatus p where
    unpack (Posix.COff a) = a


aTime' :: PathLikeEnvIO Error m p => p -> m (Either Error PosixInt)
aTime' = runEitherT . aTime

aTime :: PathLikeEnvIO Error m p => p -> EitherT Error m PosixInt
aTime p = unpack . Posix.accessTime <$> fileStatus p where
    unpack (C.CTime a) = a


-- TODO: change Posix.modificationTime to Posix.modificationTimeHiRes
modTime' :: PathLikeEnvIO Error m p => p -> m (Either Error PosixInt)
modTime' = runEitherT . modTime

modTime :: PathLikeEnvIO Error m p => p -> EitherT Error m PosixInt
modTime p = unpack . Posix.modificationTime <$> fileStatus p where
    unpack (C.CTime a) = a



statTime' :: PathLikeEnvIO Error m p => p -> m (Either Error PosixInt)
statTime' = runEitherT . statTime

statTime :: PathLikeEnvIO Error m p => p -> EitherT Error m PosixInt
statTime p = unpack . Posix.statusChangeTime <$> fileStatus p where
    unpack (C.CTime a) = a


isAbs' :: PathLikeEnvIO Error m p => p -> m (Either Error Bool)
isAbs' = runEitherT . isAbs

isAbs :: PathLikeEnvIO Error m p => p -> EitherT Error m Bool
isAbs p = (== Just Root) . view mountPoint <$> expand p



class WithPath p where
    withPath :: (Path -> Path) -> p -> p

instance WithPath Path where
    withPath = ($)

instance Functor m => WithPath (m Path) where
    withPath f = fmap f


lower :: WithPath p => p -> p
lower = withPath $ \(Path m segs) -> Path m $ fmap lowerSeg segs where
    lowerSeg = \case
        Node t -> Node $ T.toLower t
        a      -> a

upper :: WithPath p => p -> p
upper = withPath $ \(Path m segs) -> Path m $ fmap upperSeg segs where
    upperSeg = \case
        Node t -> Node $ T.toUpper t
        a      -> a


diff :: Path -> Path -> Maybe Path
diff p@(Path _ segs) p' = do
    (Path _ psegs) <- commonPrefix [p,p']
    let pfxLen      = length psegs
    (Path _ sx)    <- snd <$> splitAt pfxLen p'
    let relDiff     = length segs - pfxLen
        relSeg      = Rel . fromIntegral $ relDiff + 1
        relSegs     = if relDiff > 0 then relSeg : sx
                                     else sx
    return $ Path Nothing relSegs


absDiff :: PathLikeEnvIO Error m p => p -> p -> EitherT Error m Path
absDiff p p' = do
    ep  <- expand p
    ep' <- expand p'
    diff ep ep' & maybeToEitherT NoPathFound


splitAt :: Int -> Path -> Maybe (Path, Path)
splitAt i (Path m segs) = if length segs >= i then Just (Path m $ take i segs, Path Nothing $ drop i segs)
                                              else Nothing


single s = Path Nothing [s]


readEnv = (Env . Map.fromList . fmap (over both fromString)) <$> Environment.getEnvironment

main = do
    let p  = readPath "/tmp/fox2"
        p1 = readPath "$HOME/a"
        p2 = readPath "/b"
        p3 = (cwd // "a") // "b"
        -- p4 = aTime . upper . expand $ cwd // "a" // "B" // "$HOME"
        -- p5 = aTime . upper . expand $ cwd // "a" // "B" // "$HOME"
    env <- readEnv

    print p
    --print $ expandVars env p
    --print =<< expand p
    Right p' <- expand' p
    print $ p'
    print $ native p'

    print =<< check p

    let str = "/foo/~/bar/$FOO"
    print =<< expandHome' (readPath str)
    print =<< expand' str

    print " ------- "

    print =<< expand' p

    print =<< size' p

    --print =<< Directory.listd
    --print $ normalizeUnsafe p
    --print =<< p3
    --print =<< expand env p3
    print "end"
    -- print =<< runEitherT p4

    print             $ diff (fromString "/tmp/foo/b1") (fromString "/tmp/foo/bar/baz")
    print . fmap repr $ diff (fromString "/tmp/foo/b1") (fromString "/tmp/foo/bar/baz")
    --print . fmap (nativeFor Windows) =<< p4

    print =<< runEitherT (repr $ absDiff "$HOME" "/tmp")






instance Show (Posix.FileStatus) where show _ = "xx"
---- === utils ===

--segments :: Route seg -> [seg]
--segments (Route t s) = s


--toPath :: IsPath a => a -> Path
--toPath = convert

--toPath :: IsPath a => a -> Path
--toPath = convert

-- === Path joining ===

infixl 9 //

--(//) :: (Convertible p out, Convertible p' out, out ~ JoinResult p p', PathJoin out)
--     => p -> p' -> out
--(//) p p' = convert p `pathJoin` convert p'


(//) = pathJoin2

class PathJoin p where
    pathJoin :: p -> p -> p

instance Monad m => PathJoin (m Path) where
    pathJoin mp mp' = do
        p  <- mp
        p' <- mp'
        return $ pathJoin p p'

instance PathJoin Path where
    pathJoin = (<>)


class PathJoin2 p p' where
    pathJoin2 :: p -> p' -> p :+: p'


instance PathJoin2 Path Path where
    pathJoin2 = (<>)

instance (m ~ m', Monad m)
      => PathJoin2 (m Path) (m' Path) where
    pathJoin2 mp mp' = do
        p  <- mp
        p' <- mp'
        return $ pathJoin2 p p'

instance Monad m => PathJoin2 (m Path) Path where
    pathJoin2 p p' = p // return p'

instance Monad m => PathJoin2 Path (m Path) where
    pathJoin2 p p' = return p // p'

instance (Convertible a Path, Monad m, Functor m) => PathJoin2 (m Path) a where
    pathJoin2 p a = (<> convert a) <$> p




--myPathJoin :: Path -> Path -> Path
--myPathJoin = (<>)

--myPathJoinx = mapPath

--instance PathJoin2 (m (Path seg)) a where
--    pathJoin2 mp a = pathJoinSame mp (return $ convert a)

--pathJoinSame :: PathJoin2 a a => a -> a -> a :+: a
--pathJoinSame = pathJoin2



infixl 9 :+:
type family (:+:) a b where
    --(Path seg)  :+: (Path seg)                   = Path seg
    Path      :+: (m Path)        = m Path
    Path      :+: a                   = Path
    (m Path)  :+: (n Path)        = m Path
    (m Path)  :+: a                   = m Path

    a             :+: Path            = Path
    a             :+: Path            = Path
    a             :+: b                   = Path

type family JoinResult a b where
    JoinResult Path       (m Path)        = m Path
    JoinResult Path       a        = Path
    JoinResult (m Path)   (n Path)        = m Path
    JoinResult (m Path)   a        = m Path

    JoinResult a          Path = Path
    JoinResult a          Path = Path
    JoinResult a          b        = Path


----instance Monoid (IO Path) where
----    mempty = return mempty
----    p `mappend` p' = (<>) <$> p <*> p'

---- === instances ===



----instance Convertible a            a    where convert = Right
instance Convertible Path     Path where convert = id
--instance Convertible Path     Path where convert (Path mount segs) = Right $ Path mount $ fmap Node segs
instance Convertible String       Path where convert = readPath
--instance Convertible Node         Path where convert = Right . toPath . Node
--instance Convertible Node      Path where convert = Right . Route Relative . return

--instance Convertible Path Path where convert = Right
instance Convertible Text Path where convert = readPath
--instance Convertible Node     Path where convert = Right . Route Relative . return

--instance IsText Node     where fromText   = Node
--instance IsText Node  where fromText   = Node . fromText
--instance IsText Path where fromText s = toPath (fromText s :: Path)
--instance IsText Path where fromText s = Route Relative [Node s]


instance Monad m => Convertible String  (m Path) where convert = return . convert
----instance (m ~ n) => Convertible (m Path)     (n Path) where convert = Right
----instance Monad m => Convertible Path         (m Path) where convert = fmap return . convert
----instance Monad m => Convertible Path     (m Path) where convert = fmap return . convert
----instance Monad m => Convertible Text       (m Path) where convert = fmap return . convert
----instance Monad m => Convertible Node         (m Path) where convert = fmap return . convert
----instance Monad m => Convertible Node      (m Path) where convert = fmap return . convert

#endif
