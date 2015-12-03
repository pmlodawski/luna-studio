module Luna.Syntax.Symbol.Map where

import Prologue

import           Control.Error.Operator
import qualified Data.List                   as List
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import qualified Data.Maybe                  as Maybe
import           Luna.Syntax.AST.Arg         (Arg (Arg))
import qualified Luna.Syntax.Builder.Symbol  as SymbolBuilder
import           Luna.Syntax.Symbol.Network  (Network)
import           Luna.Syntax.Symbol.QualPath (QualPath)



type GeneralizedNetwork = Network
type SpecializedNetwork = Network


data SpecializationError = CouldNotSpecialize { errMsg :: String }
                         | SymbolNotFound     { qpath :: QualPath }
                         deriving (Show)

data DefArgs t = DefArgs { _defArgs :: [Arg t]
                         } deriving (Eq, Ord, Show)
makeLenses ''DefArgs

data CallArgs t = CallArgs
    { _positional :: [t]
    , _named      :: Map String t
    } deriving (Show)
makeLenses ''CallArgs

type Specification t = Signature CallArgs Maybe t
type Specialization t = Signature DefArgs Identity t

data Signature a f t = Signature
    { _args   :: a t
    , _result :: f t
    } deriving (Eq, Ord, Show)

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

getSpecialization :: (SymbolMonad t m, Enum t, Ord t)
                  => QualPath -> Specification t -> ExceptT SpecializationError m (Specialization t, SpecializedNetwork)
getSpecialization qpath specif = SymbolBuilder.modifyM $ \symbolMap -> do
    part <- Map.lookup qpath symbolMap <??> SymbolNotFound qpath
    case List.find (specificationMatch specif . fst) (Map.toList $ part ^. specs) of
        Just specializedNetwork -> return (symbolMap, specializedNetwork)
        Nothing -> do
            r@(newSpecialization, newNetwork) <- makeSpecialization specif $ part ^. general
            return (Map.adjust (specs %~ Map.insert newSpecialization newNetwork) qpath symbolMap, r)

makeSpecialization :: SymbolMonad t m => Specification t -> GeneralizedNetwork
                   -> ExceptT SpecializationError m (Specialization t, SpecializedNetwork)
makeSpecialization specif gen = error "Luna.Syntax.Symbol.Map.makeSpecialization: not implemented"


specificationMatch :: Enum t => Specification t -> Specialization t -> Bool
specificationMatch specif special = all typeMatch' (zip specifPositional (map unarg' specialPositional))
                                 && Maybe.fromMaybe False (all typeMatch' <$> namedArgs)
                                 && Maybe.fromMaybe True (flip typeMatch (runIdentity $ special ^. result) <$> (specif ^. result)) where
    specifPositional       = specif ^. args . positional
    (specialPositional, specialNamed) =  splitAt (length specifPositional) $ special ^. args . defArgs
    specifNamed            = specif ^. args . named
    namedArgs              = mapM (matchArg $ Map.fromList $ map unarg specialNamed) $ map arg $ Map.toList specifNamed
    unarg (Arg (Just n) a) = (n, a)
    unarg' (Arg _ a)       = a
    arg (n, a)             = Arg (Just n) a
    matchArg namedMap (Arg (Just n) a) = (a,) <$> Map.lookup n namedMap
    matchArg _        (Arg Nothing _ ) = Nothing
    typeMatch'             = uncurry typeMatch

--
-- argMatch ask comp = typesMatch unnamedArgs
--                  && typesMatch namedArgs
--                  && null remainings where
--     remainings = (ask)
--     unnamedArgs
--
--
-- --TODO[PM]
typeMatch :: Enum t => t -> t -> Bool
typeMatch t1 t2 = fromEnum t1 == fromEnum t2
