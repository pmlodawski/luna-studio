module Luna.Syntax.Symbol.Map where

import Prologue

import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Luna.Syntax.AST.Arg        (Arg)
import           Luna.Syntax.Symbol.Network (Network)



type QualPath = [String]

type GeneralizedNetwork = Network
type SpecializedNetwork = Network

newtype SpecializationError = SpecializationError { _errMsg :: String }
                            deriving (Show)

data Specification t = Specification { _args   :: [Arg t]
                                     , _result :: t
                                     } deriving (Eq, Show, Ord)

makeLenses ''Specification

type SpecializationMap t = Map (Specification t) SpecializedNetwork

data PartiallySpecializedNetwork t = PartiallySpecializedNetwork
    { _general :: GeneralizedNetwork
    , _specs   :: SpecializationMap t
    } deriving (Show)

makeLenses ''PartiallySpecializedNetwork

type SymbolMap t = Map QualPath (PartiallySpecializedNetwork t)


symbolLookup :: QualPath -> SymbolMap t -> Maybe (PartiallySpecializedNetwork t)
symbolLookup = Map.lookup

graph :: QualPath -> SymbolMap t -> Maybe GeneralizedNetwork
graph = fmap (view general) .: symbolLookup

specializations :: QualPath -> SymbolMap t -> Maybe (SpecializationMap t)
specializations = fmap (view specs) .: symbolLookup

getSpecialization :: Ord t => Specification t -> PartiallySpecializedNetwork t
                  -> Either SpecializationError SpecializedNetwork
getSpecialization specif part = maybe
    (makeSpecialization specif $ part ^. general )
    return
    (Map.lookup specif $ part ^. specs)

makeSpecialization :: Specification t -> GeneralizedNetwork
                   -> Either SpecializationError SpecializedNetwork
makeSpecialization specif gen = error "Luna.Syntax.Symbol.Map.makeSpecialization: not implemented"
