---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module TypesHandler (

newTypeModule,
newTypeClass,
newTypeFunction,
newTypeUdefined,
newTypeNamed,
newTypeVariable,
newTypeList,
newTypeTuple
) 
where

import qualified Data.Vector as Vector
import           Data.Vector   (Vector)

import qualified Types_Types as TTypes


newTypeModule   a name = do 
    putStrLn "NOT IMPLEMENTED - newTypeModule"
    return $ TTypes.Type $ Just $ Vector.fromList []


newTypeClass    a name params = do
    putStrLn "NOT IMPLEMENTED - newTypeClass"
    return $ TTypes.Type $ Just $ Vector.fromList []


newTypeFunction a name inputs outputs = do
    putStrLn "NOT IMPLEMENTED - newTypeFunction"
    return $ TTypes.Type $ Just $ Vector.fromList []


newTypeUdefined a = do
    putStrLn "NOT IMPLEMENTED - newTypeUdefined"
    return $ TTypes.Type $ Just $ Vector.fromList []


newTypeNamed    a name = do
    putStrLn "NOT IMPLEMENTED - newTypeNamed"
    return $ TTypes.Type $ Just $ Vector.fromList []


newTypeVariable a name type' = do
    putStrLn "NOT IMPLEMENTED - newTypeVariable"
    return $ TTypes.Type $ Just $ Vector.fromList []


newTypeList     a type' = do
    putStrLn "NOT IMPLEMENTED - newTypeList"
    return $ TTypes.Type $ Just $ Vector.fromList []


newTypeTuple    a types = do
    putStrLn "NOT IMPLEMENTED - newTypeTuple"
    return $ TTypes.Type $ Just $ Vector.fromList []