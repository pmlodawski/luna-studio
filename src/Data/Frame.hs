{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE FunctionalDependencies    #-}
-- {-# LANGUAGE AllowAmbiguousTypes       #-}


module Data.Frame where

import           Flowbox.Prelude hiding (cons, takeWhile, cast)
import qualified Flowbox.System.Types

import           Data.ByteString.Lazy.Char8  (split, unpack, takeWhile)
import           Data.ByteString.Lazy   (ByteString)
import qualified Data.ByteString.Lazy   as BS
import           Data.Csv               (decode, HasHeader(..))
import           Data.Either.Utils      (maybeToEither, forceEitherMsg)
import           Data.Function          (on)
import           Data.List              (sortBy, intercalate)
import qualified Data.Map               as M
import           Data.Map               (Map(..))
import           Data.Maybe             (fromMaybe)
import           Data.Proxy
import           Data.String.Utils      (strip)
import           Data.Typeable          hiding (cast)
import           Data.TypeLevel.Containers
import           Data.Variant
import qualified Data.Vector            as V
import           Data.Vector            (Vector)


import           GHC.Prim               (Constraint)
import           Text.Read              (Read, readMaybe)


------------------------------------------------------------------------
-- Should be in Data.Variant
------------------------------------------------------------------------
caseS :: Case a matches out => a -> matches -> Maybe out
caseS = secureCase . view record
------------------------------------------------------------------------

data Nullable a = Value a
                | Null
                | NA
                deriving (Show, Eq, Ord)


type family VectorsOf (lst :: [*]) :: [*] where
    VectorsOf '[]       = '[]
    VectorsOf (a ': as) = (Vector a ': VectorsOf as)


type family Nullables (lst :: [*]) :: [*] where
    Nullables '[]       = '[]
    Nullables (a ': as) = (Nullable a ': Nullables as)


newtype Column types = Column { _columnData :: Record (VectorsOf types) }
newtype Cell   types = Cell   { _cellData   :: Record types             }
makeLenses ''Column
makeLenses ''Cell


class HasColumn types a | a -> types where
    column :: Lens' a (Column types)

instance HasColumn types (Column types) where column = id


type instance Variants (Column types) = VectorsOf types
type instance Variants (Cell   types) = types


instance IsVariant (Column types) where
    variant = Column
    record  = columnData


instance IsVariant (Cell types) where
    variant = Cell
    record  = cellData


--instance Constructor v (Record (VectorsOf types)) => Constructor v (Column types) where
--    cons = Column . cons


newtype DSchema = DSchema { _fromSchema :: Map String Int } deriving (Eq)
makeLenses ''DSchema


data DataFrame types = DataFrame { _rows      :: Int
                                 , _cols      :: Int
                                 , _schema    :: DSchema
                                 , _frameData :: Vector (Column types)
                                 }
makeLenses ''DataFrame


type StdFrame  = DataFrame '[Bool, Char, Int, Double, String]
type StdFrameN = DataFrame (Nullables '[Bool, Char, Int, Double, String])

data CsvSource = FromFile   FilePath
               | FromString ByteString
               deriving (Show, Eq, Ord)


-------------------------------------------------------------------------
-- Beefed-up Read that can easily read to string (read = id in that case)
-------------------------------------------------------------------------

class TurboRead a where
    turboRead :: String -> Maybe a


instance (TurboRead a) => TurboRead (Nullable a) where
    turboRead str = case str of
        ""     -> Just NA
        "NA"   -> Just NA
        "null" -> Just Null
        s      -> Value <$> turboRead s


instance (Read a) => TurboRead a where
    turboRead = readMaybe


instance TurboRead String where
    turboRead = Just


-------------------------------------------------------------------------
-- Reading columns from Strings
-------------------------------------------------------------------------

readVals :: TurboRead t => Proxy t -> Vector String -> Maybe (Vector t)
readVals _ = sequence . V.map turboRead


class ReadColumn base (t :: [*]) where
    readColumn :: Proxy base -> Proxy t -> Vector String -> Maybe (Column base)


instance ReadColumn base '[] where
    readColumn _ _ _ = Nothing


instance (TurboRead t, Constructor (Vector t) (Column base), ReadColumn base ts)
      => ReadColumn base (t ': ts) where
    readColumn base t s = case readVals (Proxy :: Proxy t) s of
        Nothing -> readColumn base (Proxy :: Proxy ts) s
        Just (v :: Vector t) -> Just $ cons v


-------------------------------------------------------------------------
-- Indexing columns
-------------------------------------------------------------------------

class IndexColumn base (t :: [*]) where
    indexColumn :: HasColumn base col => Proxy base -> Proxy t -> Int -> col -> Maybe (Cell base)


instance IndexColumn base '[] where
    indexColumn _ _ _ _ = Nothing


instance (Constructor t (Cell base), IndexColumn base ts, WithVariantsM (MapVariant' (Vector t) t) (RecordTemplate (VectorsOf base)) Maybe t)
      => IndexColumn base (t ': ts) where
    indexColumn _ _ idx c = case caseRes of Just cell -> Just $ cons cell
                                            Nothing   -> indexColumn (Proxy :: Proxy base) (Proxy :: Proxy ts) idx c
        where caseRes = caseS (view column c) $ do match $ \(vec :: Vector t) -> (vec V.! idx)


-------------------------------------------------------------------------
-- Showing cells
-------------------------------------------------------------------------

class ShowCell' base (t :: [*]) where
    showCell' :: Proxy base -> Proxy t -> Cell base -> Maybe String


instance ShowCell' base '[] where
    showCell' _ _ _ = Nothing


instance (Show t, ShowCell' base ts, WithVariantsM (MapVariant' t String) (RecordTemplate base) Maybe String)
      => ShowCell' base (t ': ts) where
    showCell' _ _ c = case caseRes of Just str -> Just str
                                      Nothing  -> showCell' (Proxy :: Proxy base) (Proxy :: Proxy ts) c
        where caseRes = caseS c $ do match $ \(el :: t) -> show el


-- use this one to show cells:
showCell :: forall types. (ShowCell' types types) => Cell types -> Maybe String
showCell cell = showCell' (Proxy :: Proxy types) (Proxy :: Proxy types) cell


-------------------------------------------------------------------------
-- Showing column types
-------------------------------------------------------------------------

class ShowColType' base (t :: [*]) where
    showColType' :: Proxy base -> Proxy t -> Column base -> Maybe String


instance ShowColType' base '[] where
    showColType' _ _ _ = Nothing


instance (Typeable t, ShowColType' base ts, WithVariantsM (MapVariant' (Vector t) String) (RecordTemplate (VectorsOf base)) Maybe String)
      => ShowColType' base (t ': ts) where
    showColType' _ _ c = case caseRes of Just str -> Just str
                                         Nothing  -> showColType' (Proxy :: Proxy base) (Proxy :: Proxy ts) c
        where caseRes = caseS c $ do match $ \(el :: Vector t) -> show (typeOf el)


showColType :: forall types. (ShowColType' types types) => Column types -> Maybe String
showColType column = showColType' (Proxy :: Proxy types) (Proxy :: Proxy types) column


-------------------------------------------------------------------------
-- Casting columns to Double
-------------------------------------------------------------------------


class CastToDouble t where
    castToDouble :: Vector t -> Maybe (Vector Double)

instance CastToDouble Int where
    castToDouble vec = Just $ V.map (\a -> fromIntegral a :: Double) vec

instance CastToDouble Float where
    castToDouble vec = Just $ V.map (\a -> realToFrac a   :: Double) vec

instance CastToDouble Double where
    castToDouble vec = Just vec

instance CastToDouble a where
    castToDouble vec = Nothing



class ColToVec' base (t :: [*]) where
    colToVec' :: Proxy base -> Proxy t -> Column base -> Maybe (Vector Double)


instance ColToVec' base '[] where
    colToVec' _ _ _ = Nothing


instance (CastToDouble t, ColToVec' base ts, WithVariantsM
                        (MapVariant' (Vector t) (Maybe (Vector Double)))
                        (RecordTemplate (VectorsOf base))
                        Maybe
                        (Maybe (Vector Double)))
      => ColToVec' base (t ': ts) where
    colToVec' _ _ c = case caseRes of Just v  -> v
                                      Nothing -> colToVec' (Proxy :: Proxy base) (Proxy :: Proxy ts) c
        where caseRes = caseS c $ do match $ \(vec :: Vector t) -> castToDouble vec


colToVec :: forall types. (ColToVec' types types) => Column types -> Maybe (Vector Double)
colToVec column = colToVec' (Proxy :: Proxy types) (Proxy :: Proxy types) column


-------------------------------------------------------------------------
-- Filling NA values
-------------------------------------------------------------------------

class FromNullable a where
    fromNullable :: Nullable a -> a


instance FromNullable Double where
    fromNullable (Value a) = a
    fromNullable _         = (-1)


instance FromNullable Float where
    fromNullable (Value a) = a
    fromNullable _         = (-1)


instance FromNullable Int where
    fromNullable (Value a) = a
    fromNullable _         = (-1)


instance FromNullable String where
    fromNullable (Value a) = a
    fromNullable _         = ""


instance FromNullable Char where
    fromNullable (Value a) = a
    fromNullable _         = '\0'


instance FromNullable Bool where
    fromNullable (Value a) = a
    fromNullable _         = False



class FillNA' base (t :: [*]) where
    fillNA' :: Proxy base -> Proxy t -> Column (Nullables base) -> Maybe (Column base)


instance FillNA' base '[] where
    fillNA' _ _ _ = Nothing


instance (FromNullable t, FillNA' base ts, Constructor (Vector t) (Column base),
                        WithVariantsM
                        (MapVariant' (Vector (Nullable t)) (Vector t))
                        (RecordTemplate (VectorsOf (Nullables base)))
                        Maybe
                        (Vector t))
      => FillNA' base (t ': ts) where
    fillNA' _ _ c = case caseRes of Just v  -> Just $ cons v
                                    Nothing -> fillNA' (Proxy :: Proxy base) (Proxy :: Proxy ts) c
        where caseRes = caseS c $ do match $ \(vec :: Vector (Nullable t)) -> (V.map fromNullable vec)


fillNACol :: forall types. (FillNA' types types) => Column (Nullables types) -> Maybe (Column types)
fillNACol column = fillNA' (Proxy :: Proxy types) (Proxy :: Proxy types) column


fillNA :: forall types. (FillNA' types types) => DataFrame (Nullables types) -> Maybe (DataFrame types)
fillNA frame = case newData of Just d  -> Just $ frame & frameData .~ d
                               Nothing -> Nothing
    where newData = frame ^. frameData & V.map fillNACol & V.sequence


-------------------------------------------------------------------------
-- Adding columns
-------------------------------------------------------------------------

class AppendEl (el :: *) (cont :: [*]) (cont' :: [*]) | el cont -> cont' where
    appendEl :: Proxy el -> Proxy cont -> Proxy cont'

class AppendEl' (inplace :: Bool) (el :: *) (cont :: [*]) (cont' :: [*]) | inplace el cont -> cont' where
    appendEl' :: Proxy inplace -> Proxy el -> Proxy cont -> Proxy cont'


instance (inplace ~ Elem el cont, AppendEl' inplace el cont cont') => AppendEl el cont cont' where
    appendEl _ _ = appendEl' (Proxy :: Proxy inplace) (Proxy :: Proxy el) (Proxy :: Proxy cont)

instance AppendEl' 'True el cont cont where
    appendEl' _ _ _ = Proxy :: Proxy cont

instance AppendEl' 'False el cont (el ': cont) where
    appendEl' _ _ _ = Proxy :: Proxy (el ': cont)



--addCol :: forall types t. Vector t -> DataFrame

---------------------------------------------------------------------------------
---------------------------------------------------------------------------------


toCol :: forall types t tout. (AppendEl t types tout, Constructor (Vector t) (Column tout))
                         => Proxy types -> Vector t -> Column tout
toCol _ vec = cons vec


castCol :: forall tsFrom tsTo.
          (WrappedCast (Record (VectorsOf tsFrom)) (CastOutput (IsSecureCast (Record (VectorsOf tsFrom)) (Record (VectorsOf tsTo)))) (Record (VectorsOf tsTo)),
           UnpackCast  (CastOutput (IsSecureCast (Record (VectorsOf tsFrom)) (Record (VectorsOf tsTo))) (Record (VectorsOf tsTo))) (Record (VectorsOf tsTo)))
          => Proxy tsTo -> Column tsFrom -> Column tsTo
castCol _ col = Column castRes
    where cdata   = col ^. record :: Record (VectorsOf tsFrom)
          castRes = cast cdata    :: Record (VectorsOf tsTo)

--addCol :: forall types t. (Constructor (Vector t) (Column (t ': types)))
--                         => Vector t -> DataFrame types -> DataFrame (t ': types)
--addCol vec frame =
--    where newCol = toCol (Proxy :: Proxy types) vec


--addCol :: forall types t. Vector t -> DataFrame types -> DataFrame (t ': types)
--addCol

-------------------------------------------------------------------------
-- Reading frames from CSV
-------------------------------------------------------------------------

transposeFrame :: Vector (Vector a) -> Vector (Vector a)
transposeFrame data1 = V.foldl apnd vstart data1
    where vstart = V.map (\_ -> V.empty) $ V.head data1
          apnd   = V.zipWith (flip V.cons)


frameUnpack :: Vector (Vector ByteString) -> Vector (Vector String)
frameUnpack df = V.map (V.map unpack) df


readFromCsv :: ByteString -> Either String (Vector (Vector String))
readFromCsv bstr = frameUnpack <$> csvTr
    where csv   = decode HasHeader bstr :: Either String (Vector (Vector ByteString))
          csvTr = transposeFrame <$> csv


parseData :: forall types. (ReadColumn types types) => Vector (Vector String) -> Either String (Vector (Column (types::[*])))
parseData vecs = case cols of
    Just cs -> Right cs
    _       -> Left "CSV parsing error"
    where mapFun = V.map (readColumn (Proxy :: Proxy types) (Proxy :: Proxy types))
          mCols  = mapFun vecs      :: Vector (Maybe (Column types))
          cols   = V.sequence mCols :: Maybe (Vector (Column types))


readCsv :: forall types. (ReadColumn types types) => ByteString -> Either String (DataFrame types)
readCsv bstr = do

    vecs    <- readFromCsv bstr
    dfData  <- parseData vecs
    numRows <- maybeToEither "Empty dataframe" $ V.length <$> (vecs V.!? 0)

    let numCols = V.length vecs
        schema  = DSchema M.empty

    return $ DataFrame numRows numCols schema dfData


load :: forall types. (ReadColumn types types) => FilePath -> IO (Either String (DataFrame types))
load path = do
    file <- BS.readFile path
    let header = takeWhile (/= '\n') file
        sch    = DSchema $ parseCsvHeader header
        dframe = readCsv file

    print header

    case dframe of (Right df) -> return $ Right (df & schema .~ sch)
                   (Left  e ) -> return $ Left e


loadUnsafe :: forall types. (ReadColumn types types) => FilePath -> IO (DataFrame types)
loadUnsafe path = forceEitherMsg "Failed to load DataFrame from CSV" <$> load path


parseCsvHeader :: ByteString -> Map String Int
parseCsvHeader bs = M.fromList tups
    where ws   = split ',' bs & map (strip . unpack)
          tups = zip ws [0..]


-------------------------------------------------------------------------
-- Indexing frames
-------------------------------------------------------------------------

cellByIdx :: forall types. (IndexColumn types types) => Int -> Int -> DataFrame types -> Maybe (Cell types)
cellByIdx row col df = cell
    where column = df ^. frameData & (V.!? col)
          cell   = column >>= (indexColumn (Proxy :: Proxy types) (Proxy :: Proxy types) row)


cellByLabelIdx :: forall types. (IndexColumn types types) => String -> Int -> DataFrame types -> Maybe (Cell types)
cellByLabelIdx label row df = case M.lookup label (df ^. schema ^. fromSchema) of
    Just idx -> cellByIdx row idx df
    Nothing  -> Nothing


colByIdx :: forall types. Int -> DataFrame types -> Maybe (Column types)
colByIdx idx df = df ^. frameData & (V.!? idx)


colByLabel :: forall types. String -> DataFrame types -> Maybe (Column types)
colByLabel label df = case M.lookup label (df ^. schema ^. fromSchema) of
    Just idx -> colByIdx idx df
    Nothing  -> Nothing


colByIdxDouble :: forall types. (ColToVec' types types) => Int -> DataFrame types -> Maybe (Vector Double)
colByIdxDouble idx df = colByIdx idx df >>= colToVec


colByLabelDouble :: forall types. (ColToVec' types types) => String -> DataFrame types -> Maybe (Vector Double)
colByLabelDouble lab df = colByLabel lab df >>= colToVec


-------------------------------------------------------------------------
-- Showing frames
-------------------------------------------------------------------------

formatSchema :: DSchema -> [String]
formatSchema sch = l & sortBy fn & map fst
    where fn = compare `on` snd
          l  = M.toList $ sch ^. fromSchema


showFrame :: forall types. (ShowColType' types types) => DataFrame types -> String
showFrame frame = intercalate "\n" ["DataFrame:", colLabels, rowInfo, colInfo]
    where colss     = frame ^. frameData & V.map (fromMaybe "unknown" . showColType) & V.toList
          labs      = formatSchema $ frame ^. schema
          colLabs   = zipWith (\l c -> l ++ " :: " ++ c) labs colss
          colLabels = "Columns: " ++ (intercalate "; " colLabs)
          rowInfo   = "Row count: " ++ (show $ frame ^. rows)
          colInfo   = "Col count: " ++ (show $ frame ^. cols)


instance (ShowColType' types types) => Show (DataFrame types) where
    show df = showFrame df


-------------------------------------------------------------------------
-- Test
-------------------------------------------------------------------------

test = do
    let csv  = "Name,Surname,Age\nMarian,Brzoza,47\nMarianna,Olcha,45\nMaciej,Wrona,34\nOlga,Tokarczuk,12" :: ByteString
        dfe  = readCsv csv :: Either String (DataFrame '[Int, Float, String])

        df   = case dfe of
                  Right dframe -> dframe
                  Left  e      -> error e

        c2   = cellByIdx 1 2 df >>= showCell
        kol  = colByIdx  1   df >>= showColType
        vec1 = colByIdx  2   df >>= colToVec
        vec2 = colByIdx  1   df >>= colToVec

    print c2
    print kol
    print vec1
    print vec2
