---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of mthis file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Handlers.Types (

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

import           Data.Text.Lazy                (Text, unpack)
import qualified Data.Vector                 as Vector
import           Data.Vector                   (Vector)

import           Handlers.Common
import qualified Types_Types                 as TTypes
import           Luna.Tools.Conversion
import           Luna.Tools.Conversion.Defs ()
import           Luna.Type.Type                (Type(..))



newTypeModule :: b -> Maybe Text -> IO TTypes.Type
newTypeModule _ mtname = case mtname of 
    Nothing    -> throw' "`name` field is missing"
    Just tname -> return $ encode $ Module $ unpack tname


newTypeClass :: b -> Maybe Text -> Maybe (Vector TTypes.Type) -> IO TTypes.Type
newTypeClass _ mtname mtparams = case mtname of
    Nothing    -> throw' "`name` field is missing"
    Just tname -> case mtparams of 
        Nothing      -> throw' "`params` field is missing"
        Just tparams -> case decode $ Vector.toList tparams of 
            Left  message -> throw' $ "Failed to decode `params` : " ++ message
            Right aparams -> return $ encode $ Class (unpack tname) aparams


newTypeFunction :: b -> Maybe Text -> Maybe TTypes.Type -> Maybe TTypes.Type -> IO TTypes.Type
newTypeFunction _ mtname mtinputs mtoutputs = case mtname of
    Nothing    -> throw' "`name` field is missing"
    Just tname -> case mtinputs of
        Nothing      -> throw' "`inputs` field is missing"
        Just tinputs -> case decode tinputs of
            Left message  -> throw' ("Failed to decode `inputs` : " ++ message)
            Right ainputs -> case mtoutputs of 
                Nothing       -> throw' "`outputs` field is missing"
                Just toutputs -> case decode toutputs of
                    Left message   -> throw' ("Failed to decode `outputs` : " ++ message)
                    Right aoutputs -> return $ encode $ Function (unpack tname) ainputs aoutputs


newTypeUdefined :: b -> IO TTypes.Type
newTypeUdefined _ = do
    return $ encode Undefined


newTypeNamed :: b -> Maybe Text -> Maybe TTypes.Type -> IO TTypes.Type
newTypeNamed _ mtname mttype = case mtname of 
    Nothing    -> throw' "`name` field is missing"
    Just tname -> case mttype of 
        Nothing -> throw' "`type` field is missing"
        Just ttype -> case decode ttype of
            Left message -> throw' ("Failed to decode `type` : " ++ message)
            Right atype  -> return $ encode $ Named (unpack tname) atype


newTypeVariable :: b -> Maybe Text -> IO TTypes.Type
newTypeVariable _ mtname = case mtname of 
    Nothing    -> throw' "`name` field is missing"
    Just tname -> return $ encode $ TypeVariable $ unpack tname


newTypeList :: b -> Maybe TTypes.Type -> IO TTypes.Type
newTypeList _ mttype = case mttype of
    Nothing     -> throw' "`type` fields is missing"
    Just ttype -> case decode ttype of 
        Left message -> throw' ("Failed to decode `type` : " ++ message)
        Right atype -> return $ encode $ List atype


newTypeTuple :: b -> Maybe (Vector TTypes.Type) -> IO TTypes.Type
newTypeTuple _ mttypes = case mttypes of 
    Nothing     -> throw' "`types` field is missing"
    Just ttypes -> case decode $ Vector.toList ttypes of 
        Left message -> throw' ("Failed to decode `types` : " ++ message)
        Right atypes -> return $ encode $ Tuple atypes
    
 