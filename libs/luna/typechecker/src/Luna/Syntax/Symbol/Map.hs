{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}

module Luna.Syntax.Symbol.Map where

import Prologue

import           Control.Error.Operator
import qualified Data.List                   as List
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Luna.Syntax.AST.Arg         (NamedArg (NamedArg))
import           Luna.Syntax.AST.Term        (Arrow (Arrow))
import qualified Luna.Syntax.Builder.Symbol  as SymbolBuilder
import           Luna.Syntax.Name
import           Luna.Syntax.Symbol.Network  (Network)
import           Luna.Syntax.Symbol.QualPath (QualPath)


type GeneralizedNetwork = Network
type SpecializedNetwork = Network


data SpecializationError = CouldNotSpecialize { errMsg :: String }
                         | SymbolNotFound     { qpath :: QualPath }
                         deriving (Show)

data ArgLstType = Specified
                | Specialized
                deriving (Show)

type family ArgList t a where ArgList 'Specified a = Map Name a
                              ArgList 'Specialized a = [NamedArg a]

type family Result t a where Result 'Specified   a = Maybe a
                             Result 'Specialized a = a

data Args (x :: ArgLstType) t = Args
    { _positional :: [t]
    , _named      :: ArgList x t
    }

makeLenses ''Args

deriving instance (Show (ArgList x t), Show         t)  => Show (Args x t)
deriving instance (Eq   (ArgList x t), Eq           t)  => Eq   (Args x t)
deriving instance (Ord  (ArgList x t), Ord          t)  => Ord  (Args x t)
deriving instance (Show (Result  x t), Show (Args x t)) => Show (Signature x t)
deriving instance (Ord  (Result  x t), Ord  (Args x t)) => Ord  (Signature x t)
deriving instance (Eq   (Result  x t), Eq   (Args x t)) => Eq   (Signature x t)

type Specification t = Signature 'Specified t
type Specialization t = Signature 'Specialized t

data Signature (x :: ArgLstType) t = Signature
    { _args   :: Args x t
    , _result :: Result x t
    }
makeLenses ''Signature

type SpecializationMap t = Map (Specialization t) SpecializedNetwork

data PartiallySpecializedNetwork t = PartiallySpecializedNetwork
    { _general :: GeneralizedNetwork
    , _specs   :: SpecializationMap t
    } -- deriving (Show)

makeLenses ''PartiallySpecializedNetwork

type SymbolMap t = Map QualPath (PartiallySpecializedNetwork t)

type SymbolMonad t = SymbolBuilder.MonadSymbolBuilder (SymbolMap t)


fromArrow :: Arrow t -> Specification t
fromArrow (Arrow p n r) = Signature (Args p n) (Just r)
-- fromArrow :: Arrow t -> Signature Specified t

fromArrow' :: Arrow t -> Specialization t
fromArrow' (Arrow p n r) = Signature (Args p $ map mkArg $ Map.toList n) r
    where mkArg (k, v)= NamedArg k v


toArrow :: Signature 'Specialized t -> Arrow t
toArrow (Signature (Args p n) r) = Arrow p (Map.fromList $ map fromNamed n) r
    where fromNamed (NamedArg k v) = (k, v)


symbolLookup :: SymbolMonad t m => QualPath -> m (Maybe (PartiallySpecializedNetwork t))
symbolLookup qpath' = Map.lookup qpath' <$> SymbolBuilder.get

graph :: SymbolMonad t m => QualPath ->  m (Maybe GeneralizedNetwork)
graph qpath' = fmap (view general) <$> symbolLookup qpath'

specializations :: SymbolMonad t m => QualPath -> m (Maybe (SpecializationMap t))
specializations qpath' = fmap (view specs) <$> symbolLookup qpath'

getSpecialization :: (SymbolMonad t m, Ord t)
                  => QualPath -> Specification t -> ExceptT SpecializationError m (Specialization t, SpecializedNetwork)
getSpecialization qpath' specif = SymbolBuilder.modifyM $ \symbolMap -> do
    part <- Map.lookup qpath' symbolMap <??> SymbolNotFound qpath'
    case List.find (specificationMatch specif . fst) (Map.toList $ part ^. specs) of
        Just specializedNetwork -> return (symbolMap, specializedNetwork)
        Nothing -> do
            r@(newSpecialization, newNetwork) <- makeSpecialization specif $ part ^. general
            return (Map.adjust (specs %~ Map.insert newSpecialization newNetwork) qpath' symbolMap, r)

makeSpecialization :: SymbolMonad t m => Specification t -> GeneralizedNetwork
                   -> ExceptT SpecializationError m (Specialization t, SpecializedNetwork)
makeSpecialization specif gen = error "Luna.Syntax.Symbol.Map.makeSpecialization: not implemented"


specificationMatch :: Specification t -> Specialization t -> Bool
specificationMatch specif special =  error "Luna.Syntax.Symbol.Map.specificationMatch: not implemented"
    {-all typeMatch' (zip specifPositional (map unarg' specialPositional))
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
    typeMatch'             = uncurry typeMatch -}

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
