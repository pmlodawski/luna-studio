module Luna.Syntax.Symbol.Map where

import Prologue

import           Control.Error
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Luna.Syntax.AST.Arg        (Arg)
import qualified Luna.Syntax.Builder.Symbol as SymbolBuilder
import           Luna.Syntax.Symbol.Network (Network)
import qualified Data.List as List


type QualPath = [String] --TODO[PM] Use FastStrings

type GeneralizedNetwork = Network
type SpecializedNetwork = Network


data SpecializationError = CouldNotSpecialize { errMsg :: String }
                         | SymbolNotFound     { qpath :: QualPath }
                         deriving (Show)

data DefArgs t = DefArgs { _defargs :: [Arg t] }
               deriving (Eq, Show, Ord)

data CallArgs t = CallArgs
        { _unnamed :: [t]
        , _named :: Map String t
        } deriving (Eq, Show, Ord)


type Specification t = Signature CallArgs t
type Specialization t = Signature DefArgs t

data Signature a t = Signature { _args :: a t
                               , _result :: t
                               } deriving (Eq, Show, Ord)

makeLenses ''Signature
makeLenses ''CallArgs
makeLenses ''DefArgs

type SpecializationMap t = Map (Specialization t) SpecializedNetwork

data PartiallySpecializedNetwork t = PartiallySpecializedNetwork
    { _general :: GeneralizedNetwork
    , _specs   :: SpecializationMap t
    } deriving (Show)

makeLenses ''PartiallySpecializedNetwork

type SymbolMap t = Map QualPath (PartiallySpecializedNetwork t)

type SymbolMonad t = SymbolBuilder.MonadSymbolBuilder (SymbolMap t)


symbolLookup :: SymbolMonad t m => QualPath -> m (Maybe (PartiallySpecializedNetwork t))
symbolLookup qpath = Map.lookup qpath <$> SymbolBuilder.get

graph :: SymbolMonad t m => QualPath ->  m (Maybe GeneralizedNetwork)
graph qpath = fmap (view general) <$> symbolLookup qpath

specializations :: SymbolMonad t m => QualPath -> m (Maybe (SpecializationMap t))
specializations qpath = fmap (view specs) <$> symbolLookup qpath

getSpecialization :: (SymbolMonad t m, Ord t)
                  => QualPath -> Specification t -> ExceptT SpecializationError m SpecializedNetwork
getSpecialization qpath specif = SymbolBuilder.modifyM $ \symbolMap -> do
    part <- Map.lookup qpath symbolMap <??> SymbolNotFound qpath
    case snd <$> List.find (specificationMatch specif . fst) (Map.toList $ part ^. specs) of
        Just specializedNetwork -> return (symbolMap, specializedNetwork)
        Nothing -> do
            (newSpecialization, newNetwork) <- makeSpecialization specif $ part ^. general
            return (Map.adjust (specs %~ Map.insert newSpecialization newNetwork) qpath symbolMap, newNetwork)

makeSpecialization :: SymbolMonad t m => Specification t -> GeneralizedNetwork
                   -> ExceptT SpecializationError m (Specialization t, SpecializedNetwork)
makeSpecialization specif gen = error "Luna.Syntax.Symbol.Map.makeSpecialization: not implemented"


specificationMatch :: Specification t -> Specialization t -> Bool
specificationMatch = error "Luna.Syntax.Symbol.Map.specificationMatch: not implemented"
--
-- argMatch ask comp = typesMatch unnamedArgs
--                  && typesMatch namedArgs
--                  && null remainings where
--     remainings = (ask)
--     unnamedArgs
--
--
-- --TODO[PM]
-- typeMatch t1 t2 = t1 == t2
