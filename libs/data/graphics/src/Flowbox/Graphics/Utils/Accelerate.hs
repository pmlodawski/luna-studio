---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Send functions from this module as pull request to Accelerate?
module Flowbox.Graphics.Utils.Accelerate where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate.Smart
import Data.Array.Accelerate.Tuple
import Data.Array.Accelerate.Array.Sugar

import Language.Haskell.TH
import Control.Monad

import Flowbox.Prelude hiding (head, last)




index3 :: A.Exp Int -> A.Exp Int -> A.Exp Int -> A.Exp A.DIM3
index3 x y z = A.lift $ A.Z A.:. x A.:. y A.:. z

-- | Sequential map function
smap :: forall e sh. (A.Elt e, A.Shape sh) => (A.Exp e -> A.Exp e) -> A.Acc (A.Array sh e) -> A.Acc (A.Array sh e)
smap f arr = A.reshape inputSize $ A.asnd $ A.awhile condition step initialState
  where inputSize    = A.shape arr
        condition v  = A.unit $ A.the (A.afst v) A.<* n
        initialState = A.lift (A.unit $ A.constant 0, emptyVector)

        n = A.size arr

        step :: A.Acc (A.Scalar Int, A.Vector e) -> A.Acc (A.Scalar Int, A.Vector e)
        step (A.unlift -> (it, acc) :: (A.Acc (A.Scalar Int), A.Acc (A.Vector e))) =
            A.lift ( A.unit (A.the it + 1)
                   , acc A.++ A.reshape (A.index1 1) (A.unit (f $ arr A.!! A.the it))
                   )


emptyVector :: (A.Elt e) => A.Acc (A.Vector e)
emptyVector = A.use $ A.fromList (A.Z A.:. (0 :: Int)) []

head :: (A.Elt e, A.Shape sh) => A.Acc (A.Array sh e) -> A.Exp e
head vec = vec A.!! 0

last :: (A.Elt e, A.Shape sh) => A.Acc (A.Array sh e) -> A.Exp e
last vec = vec A.!! (A.size vec - 1)

-- | Requires following language pragmas to work:
--   {-# LANGUAGE MultiParamTypeClasses #-}
--   {-# LANGUAGE ScopedTypeVariables   #-}
--   {-# LANGUAGE TypeFamilies          #-}
--   {-# LANGUAGE UndecidableInstances  #-}
genFullTypeConstructor :: Name -> [TyVarBndr] -> (Name -> TypeQ) -> TypeQ
genFullTypeConstructor typeName typeParams f = wrapper (reverse typeParams)
            where wrapper []                  = conT typeName
                  wrapper (PlainTV name : xs) = appT (wrapper xs) (f name)

deriveAccelerate :: Name -> DecsQ
deriveAccelerate t = do
    TyConI (DataD _ typeName typeParams constructors _) <- reify t

    let [RecC valueConstructorName accessors] = constructors

    let fullTypeConstructor = genFullTypeConstructor typeName typeParams

    let genConstraints :: Name -> [TyVarBndr] -> (Name -> Name -> PredQ) -> [PredQ]
        genConstraints className tParams predGen = case tParams of
            []                  -> []
            (PlainTV name : xs) -> predGen className name : genConstraints className xs predGen

    let eltReprTuple = wrapper (length accessors) (reverse accessors)
            where wrapper alen accsr' = case accsr' of
                    []                    -> tupleT alen
                    (name, _, _type) : xs -> appT (wrapper alen xs) (return _type)

    let tFullType = fullTypeConstructor varT
    
    let eltReprFamily =  [d|type instance EltRepr $tFullType = EltRepr $eltReprTuple
                            type instance EltRepr' $tFullType = EltRepr' $eltReprTuple
                           |]

    let constrainInstance :: Cxt -> [Dec] -> DecsQ
        constrainInstance constr [InstanceD _ t d] = return . return $ InstanceD constr t d

    let paramNames = fmap (\(n, _) -> mkName [n]) $ zip ['a'..] accessors

    let patternTuple = tupP $ fmap varP paramNames
    let expressionTuple = tupE $ fmap varE paramNames
    let patternData = conP valueConstructorName $ fmap varP paramNames
    let expressionData = wrapper (reverse paramNames)
            where wrapper accsr = case accsr of
                    []          -> conE valueConstructorName
                    (name : xs) -> appE (wrapper xs) (varE name)

    eltConstr <- cxt $ genConstraints ''Elt typeParams $ \className name -> classP className [varT name]
    let eltInstance = [d| instance Elt $tFullType where 
                                eltType _ = eltType (undefined :: $eltReprTuple)
                                eltType' _ = eltType (undefined :: $eltReprTuple)
                                toElt p = case toElt p of { $patternTuple -> $expressionData }
                                toElt' p = case toElt' p of { $patternTuple -> $expressionData }
                                fromElt $patternData = fromElt $expressionTuple
                                fromElt' $patternData = fromElt' $expressionTuple
                        |] >>= constrainInstance eltConstr

    let isTupleInstance = [d| instance IsTuple $tFullType where
                                    type TupleRepr $tFullType = TupleRepr $eltReprTuple
                                    fromTuple $patternData = fromTuple $expressionTuple
                                    toTuple t = case toTuple t of { $patternTuple -> $expressionData }
                            |]

    eltPlainConstr <- cxt $ genConstraints ''Elt typeParams $ 
        \className name -> classP className [appT (conT ''A.Plain) $ varT name]

    liftExpConstr <- cxt $ genConstraints ''A.Lift typeParams $ 
        \className name -> classP className [conT ''A.Exp, varT name]

    let tFullPlainType = fullTypeConstructor (\name -> appT (conT ''A.Plain) $ varT name)

    let accTupleLift = wrapper (reverse paramNames)
            where wrapper accsr = case accsr of
                    []          -> conE 'NilTup
                    (name : xs) -> infixE (Just $ wrapper xs) 
                                          (conE 'SnocTup)
                                          (Just $ appE (varE 'A.lift) (varE name))

    let liftInstance = [d| instance A.Lift A.Exp $tFullType where
                                type Plain $tFullType = $tFullPlainType
                                lift $patternData = Exp $ Tuple $ $accTupleLift
                         |] >>= constrainInstance (eltPlainConstr ++ liftExpConstr)


    let genEqConstraints p = case p of
            []                      -> []
            ((n1, PlainTV n2) : xs) -> equalP (varT n2) (appT (conT ''A.Exp) (varT n1)) : genEqConstraints xs
    
    let typeParamNames = zip (fmap (mkName . pure) ['a'..]) typeParams
    expEqConstr <- cxt $ genEqConstraints typeParamNames
    unliftEltConstr <- cxt $ genConstraints ''Elt (fmap (PlainTV . fst) typeParamNames) $
        \className name -> classP className [varT name]
    

    let genUnlift :: Name -> ExpQ
        genUnlift t = wrapper $ zip accessors [0 .. length accessors]
            where genTupIdx 0 = conE 'ZeroTupIdx
                  genTupIdx n = appE (conE 'SuccTupIdx) (genTupIdx $ n - 1)
                  wrapper [] = conE valueConstructorName
                  wrapper (((name, _, _), nr) : xs) = 
                        appE (wrapper xs) (appE (conE 'Exp) (infixE (Just $ genTupIdx nr)
                                                                      (conE 'Prj)
                                                                      (Just $ varE t)))

    let unliftInstance = [d| instance A.Unlift A.Exp $tFullType where
                                unlift t = $(genUnlift 't)
                           |] >>= constrainInstance (expEqConstr ++ unliftEltConstr)

    let quotes = [ eltReprFamily
                 , eltInstance
                 , isTupleInstance
                 , liftInstance
                 , unliftInstance
                 ]

    foldM (fmap . (++)) [] quotes


-- | Requires following language pragmas to work:
--   {-# LANGUAGE FlexibleInstances  #-}
deriveEach :: Name -> DecsQ
deriveEach t = do
    TyConI (DataD _ typeName typeParams constructors _) <- reify t
    when (length typeParams > 1) $ error "Unable to derive instance for the type having more than one parameter!"

    let fullTypeConstructor = genFullTypeConstructor typeName typeParams
    let tFullType = fullTypeConstructor varT

    let [RecC valueConstructorName accessors] = constructors
    let paramNames = fmap (\(n, _) -> mkName [n]) $ zip ['a'..] accessors
    let patternData = conP valueConstructorName $ fmap varP paramNames

    let valueConstructor = conE valueConstructorName

    let genEach f = wrapper $ reverse $ mkName . pure . fst <$> zip ['a'..] accessors
            where wrapper (name : []) = infixE (Just valueConstructor)
                                               (varE '(<$>))
                                               (Just $ appE (varE f) (varE name))
                  wrapper (name : xs) = infixE (Just $ wrapper xs)
                                               (varE '(<*>))
                                               (Just $ appE (varE f) (varE name))

    let freeVariable = varT $ mkName "fr"
    let singularType = appT (conT typeName) freeVariable 
    [d| instance Each $singularType $singularType $freeVariable $freeVariable where
            each f $patternData = $(genEach 'f)
            {-# INLINE each #-}
      |]
