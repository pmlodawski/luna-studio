module Luna.Syntax.Symbol.Map where

import Prologue

import           Control.Error
import qualified Data.List                  as List
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import qualified Data.Maybe                 as Maybe
import           Luna.Syntax.AST.Arg        (Arg (Arg))
import qualified Luna.Syntax.Builder.Symbol as SymbolBuilder
import           Luna.Syntax.Symbol.Network (Network)



type QualPath = [String] --TODO[PM] Use FastStrings

type GeneralizedNetwork = Network
type SpecializedNetwork = Network


data SpecializationError = CouldNotSpecialize { errMsg :: String }
                         | SymbolNotFound     { qpath :: QualPath }
                         deriving (Show)

data DefArgs t = DefArgs { _defargs :: [Arg t] }
               deriving (Eq, Show, Ord)
makeLenses ''DefArgs

data CallArgs t = CallArgs
        { _positional :: [t]
        , _named      :: Map String t
        } deriving (Eq, Show, Ord)
makeLenses ''CallArgs

type Specification t = Signature CallArgs t
type Specialization t = Signature DefArgs t

data Signature a t = Signature { _args   :: a t
                               , _result :: t
                               } deriving (Eq, Show, Ord)

makeLenses ''Signature

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
specificationMatch specif special = all typeMatch' (zip specifPositional (map unarg specialPositional))
                                 && Maybe.fromMaybe False (all typeMatch' <$> namedArgs)
                                 && typeMatch (specif ^. result) (special ^. result) where
    specifPositional = specif ^. args . positional
    (specialPositional, specialNamed) =  splitAt (length specifPositional) $ special ^. args . defargs
    specifNamed = specif ^. args . named
    namedArgs = mapM (matchArg specifNamed) specialNamed
    unarg (Arg _ a) = a
    matchArg namedMap (Arg (Just n) a) = (a,) <$> Map.lookup n namedMap
    matchArg _        (Arg Nothing _ ) = Nothing
    typeMatch' = uncurry typeMatch

--
-- argMatch ask comp = typesMatch unnamedArgs
--                  && typesMatch namedArgs
--                  && null remainings where
--     remainings = (ask)
--     unnamedArgs
--
--
-- --TODO[PM]
typeMatch t1 t2 = error "Luna.Syntax.Symbol.Map.typeMatch: not implemented"
