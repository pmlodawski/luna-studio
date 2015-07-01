

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
{-# LANGUAGE TupleSections            #-}
{-# LANGUAGE StandaloneDeriving            #-}
{-# LANGUAGE DefaultSignatures            #-}
{-# LANGUAGE FlexibleInstances            #-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE PolyKinds #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Flowbox.System.Path where

--import           Control.Monad      ((>=>))
import           Data.Binary        (Binary)
import           Data.Typeable      hiding (cast)
--import           Data.List.Split    (splitOn)
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
--import           Control.Error.Safe (justErr)
--import           Data.Binary
import           Data.Monoid
import           Data.Maybe (fromJust)
import           Data.Foldable (fold)

--import           Flowbox.Prelude                    hiding (empty, fromList, toList)
--import qualified Flowbox.System.Directory.Locations as Directory
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import Unsafe.Coerce (unsafeCoerce)
--import Flowbox.Prelude hiding (cons)
import GHC.TypeLits
import Data.Default

--import Control.Monad.Shuffle (deepBind, (>>>=), ($>>=))

--import Data.ConcreteTypeRep

import qualified Data.Map as Map
--import Control.Monad.Trans.Either
import Data.List (intercalate)
import GHC.Exts (Constraint)
import Control.Monad.State hiding (withState)
import Control.Applicative hiding (empty)

--------------------------

import           Data.List.Split    (splitOn)
import           GHC.Generics       (Generic)

import Control.Monad.Identity

import Flowbox.System.Variants hiding (main, cast)
import qualified Flowbox.System.Variants as Variants

import Control.Monad.Trans.Either (EitherT(EitherT), runEitherT, hoistEither)
import qualified Control.Monad.Trans.Either as Either
import Control.Monad.IO.Class
import Flowbox.System.Types

#include "ghcplatform.h"


isEmpty = (== mempty)

---------------------------------

class MonadEither l m | m -> l where
    left :: l -> m a

instance Monad m => MonadEither l (EitherT l m) where
    left = Either.left

right :: (MonadEither l m, Monad m) => a -> m a
right = return


instance (MonadEither e m, Monad m) => MonadEither e (StateT s m) where
    left = lift . left

liftEither :: (MonadEither l m, Monad m) => Either l r -> m r
liftEither = \case
    Left  l -> left l
    Right r -> return r

-- ?? Why it is not visible?
instance MonadIO m => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right
----------------------------------------------------


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


-------------------------------------------------------------------------------------------------------------------

--type Variants vars = (IsSet vars, WithVariants' VariantRawShow (RecordTemplate vars) String)
------------------------------------------------------------------------
-- Path
------------------------------------------------------------------------

data Error = UnknownSpecial String
           | UnknownVar     String
           | NegativePath
           deriving (Show)


data Segment segs = Root
                  | Node String
                  | Segment (Record segs)

deriving instance Eq (Record segs) => Eq (Segment segs)

instance VariantShow (Record segs) => Show (Segment segs) where
    showsPrec d = \case
        Root      -> showString "Root"
        Node    s -> parensed $ showString "Node " . showsPrec (app_prec + 1) s
        Segment r -> parensed $ showString $ "Segment (" <> variantShow r <> ")"
        where app_prec = 10
              parensed = showParen (d > app_prec)

newtype Path (base :: [*]) (segs :: [*]) = Path { fromPath :: [Segment segs] } deriving (Typeable, Monoid, Default)

single :: Segment segs -> Path base segs
single a = Path [a]

type instance Variants (Path base segs) = segs

pathBase :: Path base segs -> Proxy base
pathBase _ = Proxy

forceBase :: Proxy base -> Path base segs -> Path base segs
forceBase _ = id

deriving instance VariantShow (Record segs) => Show (Path base segs)

--instance (Functor m, Monad m, IsSet segs, WithVariantsM tp (Record segs) m (Record segs'))
--      => WithVariantsM tp (Path base segs) m (Path base segs') where
--    withVariantsM tp = withSegmentsM (withVariantsM tp)

--instance (Monad m, WithVariantsM tp a m a') => WithVariantsM tp [a] m [a'] where
--    withVariantsM = mapM . withVariantsM

--instance (Monad m, Functor m, IsSet segs, WithVariantsM tp (Record segs) m (Record segs'))
--      => WithVariantsM tp (Segment segs) m (Segment segs') where
--    withVariantsM tp = \case
--        Root      -> return Root
--        Node    s -> return $ Node s
--        Segment s -> Segment <$> withVariantsM tp s

withSegments :: ([Segment segs] -> [Segment segs']) -> Path base segs -> Path base segs'
withSegments f = Path . f . fromPath

withSegmentsM :: Functor m => ([Segment segs] -> m [Segment segs']) -> Path base segs -> m (Path base segs')
withSegmentsM f = fmap Path . f . fromPath

deriving instance Eq (Segment segs) => Eq (Path base segs)
--deriving instance Variants segs => Show (Path base segs)



nonEmpty = filter (/= mempty)

readPath :: RecordReader segs => String -> Either Error (Path base segs)
readPath str@(s:_)  = fmap (Path . mount . concat) . sequence $ fmap (Right . readPathSegment) segs where
    mount     = if s == '/' then (Root:) else id
    segs      = nonEmpty $ splitOn "/" str
    --readSeg s = if and $ fmap (=='.') s
--        then Right $ replicate (length s - 1) Up
--        else fmap return $ case s of
--            --"~"    -> Right home
--            (a:as) -> case a of
--                '$' -> Right $ Var as
--                --'@' -> readSpecial as
--                _   -> Right . Segment $ Node s

type RecordReader s = RecordReader' s s

---

class RecordReader' (s :: [*]) rs where
    readRecord :: Proxy s -> String -> Maybe [Record rs]

instance (PathSegment seg, RecordReader' segs r, Constructor seg r)
      => RecordReader' (seg ': segs) r where
    readRecord _ s = case (readSegment s :: Maybe [seg]) of
        Just x  -> Just $ fmap cons x
        Nothing -> readRecord (Proxy :: Proxy segs) s

instance RecordReader' '[] r where
    readRecord _ _ = Nothing

---

class PathSegment s where
    readSegment :: String -> Maybe [s]
    showSegment :: s -> String



readPathSegment :: forall (segs :: [*]). RecordReader segs => String -> [Segment segs]
readPathSegment s = case readRecord (Proxy :: Proxy segs) s of
    Just s  -> fmap Segment s
    Nothing -> [Node s]




---

data Up = Up deriving (Show, Eq, Typeable)
type instance NatRep Up = '[0]

instance PathSegment Up where
    readSegment s = if and $ fmap (=='.') s
        then Just $ replicate (length s - 1) Up
        else Nothing
    showSegment _ = ".."

instance SegmentExpander sys Up m base segs where
    expandSegment _ p = do
        expPath <- get
        when (isEmpty expPath) $ left NegativePath
        put $ withSegments init expPath
        return mempty

---


data Temp = Temp Locality Persistence
             deriving (Show, Eq, Typeable)

data Locality = Global
              | Local
              deriving (Show, Eq, Generic)

data Persistence = Persistent
                 | NonPersistent
                 deriving (Show, Eq, Typeable)


type instance NatRep Temp = '[2]

instance PathSegment Temp where
    readSegment = readSpecial "temp" [Temp Local NonPersistent]
    showSegment _ = showSpecial "temp"

readSpecial s val = \case
    ('@' : str) -> if s == str then Just val
                               else Nothing
    _           -> Nothing

showSpecial = ('@':)

---


data Var = Var String deriving (Show, Eq, Typeable)
type instance NatRep Var = '[1]

instance PathSegment Var where
    readSegment = \case
        ('$':s) -> Just [Var s]
        _       -> Nothing
    showSegment (Var s) = '$' : s




instance (MonadIO m, RecordReader base, (WithVariantsM
                        (ExpandDiffSegment base) (RecordTemplate base) m (Path base segs))) => SegmentExpander sys Var m base segs where
    expandSegment _ (Var v) = do
        envVar  <- liftIO $ Env.getEnv v
        envPath <- liftEither $ readPath envVar :: m (Path base base)
        expandPathT' envPath
        --liftEither expPath
        return $ Path []

---

--data Expand (segs :: [*]) = Expand

--instance (selected ~ In a segs, SegmentCondExpander selected a m (Path base r), Functor m)
--      => WithVariantM (Expand segs) a m (Path base r) where
--    withVariantM _ = expandSegmentCond (Proxy :: Proxy selected)

--class SegmentCondExpander (cond :: Bool) a m r where
--    expandSegmentCond :: Proxy cond -> a -> m r

--instance (VariantCons r a, Monad m) => SegmentCondExpander False a m (Path base r) where
--    expandSegmentCond _ s = return $ Path [Segment $ cons s]

--instance (SegmentExpander Platform a m r, MonadState (Path base r) m, MonadEither Error m, Functor m)
--      => SegmentCondExpander True a m (Path base r) where
--    expandSegmentCond _ = expandSegment platform

type DiffExpander a m base r = WithVariantsM (ExpandDiffSegment a) (RecordTemplate a) m (Path base r)


class ExpandDiffSegment (segs :: [*]) seg m out where
    expandDiffSegment :: Proxy segs -> seg -> m out

instance (ExpandDiffSegmentCond selected seg m out, diff ~ Diff segs (Variants out), selected ~ In seg diff)
      => ExpandDiffSegment segs seg m out where
    expandDiffSegment _ = expandDiffSegmentCond (Proxy :: Proxy selected)

---

class ExpandDiffSegmentCond (selected :: Bool) seg m out where
    expandDiffSegmentCond :: Proxy selected -> seg -> m out

instance (Constructor seg r, Monad m) => ExpandDiffSegmentCond False seg m (Path base r) where
    expandDiffSegmentCond _ s = return $ Path [Segment $ cons s]

instance (SegmentExpanderCtx (Path base r) m, SegmentExpander Platform seg m base r) =>
         ExpandDiffSegmentCond True seg m (Path base r) where
    expandDiffSegmentCond _ = expandSegment platform


expandDiffVariants :: (WithVariantsM
                       (ExpandDiffSegment a) (RecordTemplate a) m (Path base r)) => Record a -> m (Path base r)
expandDiffVariants (r :: Record a) = withVariantsM (Proxy :: Proxy (ExpandDiffSegment a)) (expandDiffSegment (Proxy :: Proxy a)) r

type SegmentExpanderCtx s m = (Monad m, Functor m, MonadState s m, MonadEither Error m)

--fxx = withVariantsM (Proxy :: Proxy (SegmentExpander Linux)) (expandSegment Linux)

class SegmentExpander sys s m base segs where
    expandSegment :: SegmentExpanderCtx (Path base segs) m
                  => sys -> s -> m (Path base segs)


type Expand base p m p' = (Monad m, Functor m, DiffExpander p (StateT (Path base p') (EitherT Error m)) base p')

--expandPathT :: Expand base p m p'
--            => Path base p -> EitherT Error m (Path base p')
expandPathT path = flip execStateT (def :: Path base p')
                 $ mapM_ expandPathSegment segments

    where segments = fromPath path
          base     = pathBase path

          expandNewSegments = \case
              Root      -> return root
              Node    s -> return $ node s
              Segment s -> expandDiffVariants s

          expandPathSegment p = do
              expPath          <- expandNewSegments p
              Path pathBuilder <- get
              put $ Path (pathBuilder <> fromPath expPath)
              return $ forceBase base expPath

expandPathT' path = mapM_ expandPathSegment segments

    where segments = fromPath path
          base     = pathBase path

          expandNewSegments = \case
              Root      -> return root
              Node    s -> return $ node s
              Segment s -> expandDiffVariants s

          expandPathSegment p = do
              expPath          <- expandNewSegments p
              Path pathBuilder <- get
              put $ Path (pathBuilder <> fromPath expPath)
              return $ forceBase base expPath

--expandPath :: Expand base p m p'
--           => Path base p -> m (Either Error (Path base p'))
expandPath = runEitherT . expandPathT

type ExpandSingle v = (p' ~ Remove v p, Expand base p m p') => Path base p -> m (Either Error (Path base p'))


root :: Path base segs
root = single Root

node :: String -> Path base segs
node = single . Node

expandUp :: ExpandSingle Up
expandUp = expandPath

expandVar :: ExpandSingle Var
expandVar = expandPath

main = do
    let --p = var "foo" :: Segment '[Var]
        p2 = readPath "@temp/bar/$foo/.." :: (Either Error (Path '[Up, Temp, Var] '[Up, Temp, Var]))
        p3 = readPath "bar/$HOME/.." :: (Either Error (Path '[Up, Temp, Var] '[Up, Temp, Var]))

    print p2
    print p3

    case p3 of
        Left  _ -> return ()
        Right p -> do
            print =<< expandUp p
            print =<< expandVar p
            return ()
    print "end"




---




    --type RelPath = Path '[Var]


    --nonEmpty = filter (/= mempty)


    --readPath :: RecordReader segs => String -> Either Error (Path base segs)
    --readPath str@(s:_)  = fmap (Path . mount . concat) . sequence $ fmap (Right  . readPathSegment) segs where
    --    mount     = if s == '/' then (Root:) else id
    --    segs      = nonEmpty $ splitOn "/" str
    --    --readSeg s = if and $ fmap (=='.') s
    ----        then Right $ replicate (length s - 1) Up
    ----        else fmap return $ case s of
    ----            --"~"    -> Right home
    ----            (a:as) -> case a of
    ----                '$' -> Right $ Var as
    ----                --'@' -> readSpecial as
    ----                _   -> Right . Segment $ Node s

    ----class SegmentsReader s where
    ----    readSegments :: String -> Maybe (Record s)

    ----instance SegmentsReader segs => SegmentsReader (seg ': segs) where
    ----    readSegments s = case (readSegment s :: Maybe seg) of
    ----        Just x -> Just $ cons x
    ----        _      -> readSegments

    ----type VariantCons nsyIdx (Index (NatRep t) (VariantReprsOf r)) t r

    -----

    --data VariantSegmentShow = VariantSegmentShow
    --type SegmentShowCtx a   = WithVariants VariantSegmentShow (RecordTemplate a) String

    --instance (PathSegment a, Monad m) => WithVariantM VariantSegmentShow a m String where
    --    withVariantM _ = return . showSegment

    -----


    --showPath :: (IsSet segs, SegmentShowCtx segs) => Path base segs -> String
    --showPath (Path segs) = intercalate "/" $ fmap showSeg segs where
    --    showSeg = \case
    --        Root      -> "/"
    --        Node    s -> s
    --        Segment s -> withVariants VariantSegmentShow s

    --type RecordReader s = RecordReader' s s

    --class RecordReader' (s :: [*]) rs where
    --    readRecord :: Proxy s -> String -> Maybe [Record rs]

    --instance (VariantCons r seg, PathSegment seg, RecordReader' segs r) => RecordReader' (seg ': segs) r where
    --    readRecord _ s = case (readSegment s :: Maybe [seg]) of
    --        Just x  -> Just $ fmap cons x
    --        Nothing -> readRecord (Proxy :: Proxy segs) s

    --instance RecordReader' '[] r where
    --    readRecord _ _ = Nothing

    --class PathSegment s where
    --    readSegment :: String -> Maybe [s]
    --    showSegment :: s -> String



    --readPathSegment :: forall (segs :: [*]). RecordReader segs => String -> [Segment segs]
    --readPathSegment s = case readRecord (Proxy :: Proxy segs) s of
    --    Just s  -> fmap Segment s
    --    Nothing -> [Node s]


    -----

    --data Up = Up deriving (Show, Eq, Typeable)
    --type instance NatRep Up = '[0]

    --instance PathSegment Up where
    --    readSegment s = if and $ fmap (=='.') s
    --        then Just $ replicate (length s - 1) Up
    --        else Nothing
    --    showSegment _ = ".."

    --instance SegmentExpander sys Up m segs where
    --    expandSegment _ p = do
    --        expPath <- get
    --        when (isEmpty expPath) $ left NegativePath
    --        put $ withSegments init expPath
    --        return mempty

    -----

    --data Var = Var String deriving (Show, Eq, Typeable)
    --type instance NatRep Var = '[1]

    --instance PathSegment Var where
    --    readSegment = \case
    --        ('$':s) -> Just [Var s]
    --        _       -> Nothing
    --    showSegment (Var s) = '$' : s


    ----instance SegmentExpander sys Var m segs where
    ----    expandSegment _ (Var v) = do
    ----        envVar <- Env.getEnv v
    ----        envPath <-
    ----        when (isEmpty expPath) $ left NegativePath
    ----        put $ withSegments init expPath
    ----        return mempty

    --                    --Var v -> do
    --                    --    envVal  <- hoistEither $ justErr (UnknownVar v) (Map.lookup v env)
    --                    --    envPath <- hoistEither $ readRelPath envVal
    --                    --    expPath <- expandPath' envPath
    --                    --    expandSegments (expPath // path) ssPath


    -----

    --data Temp = Temp Locality Persistence
    --             deriving (Show, Eq, Typeable)

    --data Locality = Global
    --              | Local
    --              deriving (Show, Eq, Generic)

    --data Persistence = Persistent
    --                 | NonPersistent
    --                 deriving (Show, Eq, Typeable)


    --type instance NatRep Temp = '[2]

    --instance PathSegment Temp where
    --    readSegment = readSpecial "temp" [Temp Local NonPersistent]
    --    showSegment _ = showSpecial "temp"

    --readSpecial s val = \case
    --    ('@' : str) -> if s == str then Just val
    --                               else Nothing
    --    _           -> Nothing

    --showSpecial = ('@':)

    -----


    --type Expand base p m p' = ( Monad m, Functor m, IsSet p
    --                         , WithVariantsM (Expand (Diff p p')) (Record p) (StateT (Path base p') (EitherT Error m)) (Path base p')
    --                         )


    --data Expand (segs :: [*]) = Expand

    --instance (selected ~ In a segs, SegmentCondExpander selected a m (Path base r), Functor m)
    --      => WithVariantM (Expand segs) a m (Path base r) where
    --    withVariantM _ = expandSegmentCond (Proxy :: Proxy selected)

    --class SegmentCondExpander (cond :: Bool) a m r where
    --    expandSegmentCond :: Proxy cond -> a -> m r

    --instance (VariantCons r a, Monad m) => SegmentCondExpander False a m (Path base r) where
    --    expandSegmentCond _ s = return $ Path [Segment $ cons s]

    --instance (SegmentExpander Platform a m r, MonadState (Path base r) m, MonadEither Error m, Functor m)
    --      => SegmentCondExpander True a m (Path base r) where
    --    expandSegmentCond _ = expandSegment platform




    --class SegmentExpander sys s m segs where
    --    expandSegment :: (Monad m, Functor m, MonadState (Path base segs) m, MonadEither Error m)
    --                  => sys -> s -> m (Path base segs)

    --data ExpandState base segs = ExpandState { _expanded :: Path base segs }

    --deriving instance Variants segs => Show (ExpandState base segs)

    ---- todo: ponizej segs zmienic na pusta liste!
    --instance Default (ExpandState base segs) where
    --    def = ExpandState def

    --expandPath :: forall p m p' base. Expand base p m p' => Path base p -> m (Either Error (Path base p'))
    --expandPath (Path segs) = runEitherT
    --                       $ flip execStateT (def :: Path base p')
    --                       $ mapM_ expandPathSegment segs where
    --    expandNewSegments = \case
    --        Root      -> return $ Path [Root]
    --        Node    s -> return $ Path [Node s]
    --        Segment s -> withVariantsM mkExpand s

    --    expandPathSegment p = do
    --        expPath <- expandNewSegments p
    --        Path pathBuilder <- get
    --        put . Path $ pathBuilder <> fromPath expPath
    --        return (expPath :: Path base p')

    --    mkExpand = Expand :: Expand (Diff p p')


    ----class PathJoin a b | a b -> c where
    ----    pathJoin :: Path base a -> Path base b -> Path base c

    ----pathJoin :: Path base a -> Path base b -> Path base (Union a b)
    ----pathJoin p p' =



    ----type ExpandSingle v = (p' ~ Remove v p, ExpandCtx base p m p') => Path base p -> m (Either Error (Path base p'))

    ----expandUp :: ExpandSingle Up
    ----expandUp = expandPath

    ----expandVar :: ExpandSingle Var
    ----expandVar = expandPath


    -----

    ----instance

    --class Castx p p' where
    --    cast :: p -> p'


    ----instance Cast (Path base p) (Path base p') where
    ----    cast = \case

    --instance (IsSet p, Cast (Record p) (Record p')) => Castx (Segment p) (Segment p') where
    --    cast = \case
    --        Root -> Root
    --        Node s -> Node s
    --        Segment s -> Segment $ Variants.cast s

    --deriving instance Typeable '[]
    --deriving instance Typeable '(:)

    --main = do


    --    let --p = var "foo" :: Segment '[Var]
    --        p2 = readPath "@temp/bar/$foo/.." :: (Either Error (Path '[Up, Var, Temp] '[Up, Var, Temp]))
    --        p3 = readPath "bar/$foo/.." :: (Either Error (Path '[Up, Var, Temp] '[Up, Var]))

    --    --print p
    --    print p2

    --    case p2 of
    --        Left _  -> return ()
    --        Right p -> do
    --            print $ showPath p
    --            --(print . fmap showPath) =<< expandUp p
    --            --(print . fmap showPath) =<< expandVar p
    --            --(print . showPath) $ runIdentity $ expandUp p
    --            return ()

    --    case p3 of
    --        Left _  -> return ()
    --        Right p -> do
    --            --let px = cast p :: Maybe (Path '[Up, Var, Temp] '[Up, Var, Temp])
    --            print $ showPath p
    --            --print $ px
    --            --(print . fmap showPath) =<< expandUp p
    --            --(print . fmap showPath) =<< expandVar p
    --            --(print . showPath) $ runIdentity $ expandUp p
    --            return ()

    --    --let f = Foo (R1_V1 (1::Int)) :: Foo '[Int]

    --    print "end"

    ---- TODO - DOKONCZYC STATE


