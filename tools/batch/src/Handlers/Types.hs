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

import           Control.Exception
import           Data.Text.Lazy                (Text, unpack)
import qualified Data.Vector                 as Vector
import           Data.Vector                   (Vector)

import           Handlers.Common
import qualified Types_Types                 as TTypes
import           Luna.Tools.Serialization
import           Luna.Tools.Serialization.Defs ()
import           Luna.Type.Type                (Type(..))



newTypeModule :: b -> Maybe Text -> IO TTypes.Type
newTypeModule _ mtname = case mtname of 
    Just tname -> return $ encode $ Module $ unpack tname
    Nothing    -> throw' "`name` field is missing"
                  

newTypeClass :: b -> Maybe Text -> Maybe (Vector TTypes.Type) -> IO TTypes.Type
newTypeClass _ mtname mtparams = case (mtname, mtparams) of
    (Just tname, Just tparams) -> case decode $ Vector.toList tparams of 
                                Right aparams -> return $ encode $ Class (unpack tname) aparams
                                Left  message -> throw' ("Failed to decode `params` : " ++ message)
    (Just _    , Nothing     ) -> throw' "`params` field is missing"
    (Nothing   , _           ) -> throw' "`name` field is missing"


newTypeFunction :: b -> Maybe Text -> Maybe TTypes.Type -> Maybe TTypes.Type -> IO TTypes.Type
newTypeFunction _ mtname mtinputs mtoutputs = case (mtname, mtinputs, mtoutputs ) of
    (Just tname, Just tinputs, Just toutputs) -> case (decode tinputs, decode toutputs) of
                               (Right ainputs, Right aoutputs) -> return $ encode $ Function (unpack tname) ainputs aoutputs
                               (Right _      , Left message  ) -> throw' ("Failed to decode `outputs` : " ++ message)
                               (Left message , _             ) -> throw' ("Failed to decode `inputs` : " ++ message)
    (Just _    , Just _      , Nothing      ) -> throw' "`outputs` field is missing"
    (Just _    , Nothing     , _            ) -> throw' "`inputs` field is missing"
    (Nothing   , _           , _            ) -> throw' "`name` field is missing"


newTypeUdefined :: b -> IO TTypes.Type
newTypeUdefined _ = do
    return $ encode Undefined


newTypeNamed :: b -> Maybe Text -> Maybe TTypes.Type -> IO TTypes.Type
newTypeNamed _ mtname mttype = case (mtname, mttype) of 
    (Just tname, Just ttype) -> case decode ttype of
                                Right atype  -> return $ encode $ Named (unpack tname) atype
                                Left message -> throw' ("Failed to decode `type` : " ++ message)
    (Just _    , Nothing   ) -> throw' "`type` field is missing"
    (Nothing   , _         ) -> throw' "`name` field is missing"


newTypeVariable :: b -> Maybe Text -> IO TTypes.Type
newTypeVariable _ mtname = case mtname of 
    Just tname -> return $ encode $ TypeVariable $ unpack tname
    Nothing    -> throw' "`name` field is missing"


newTypeList :: b -> Maybe TTypes.Type -> IO TTypes.Type
newTypeList _ mttype = case mttype of
    Just ttype -> case decode ttype of 
                                Right atype -> return $ encode $ List atype
                                Left message -> throw' ("Failed to decode `type` : " ++ message)
    Nothing     -> throw' "`type` fields is missing"


newTypeTuple :: b -> Maybe (Vector TTypes.Type) -> IO TTypes.Type
newTypeTuple _ mttypes = case mttypes of 
    Just ttypes -> case decode $ Vector.toList ttypes of 
                                Right atypes -> return $ encode $ Tuple atypes
                                Left message -> throw' ("Failed to decode `types` : " ++ message)
    Nothing     -> throw' "`types` field is missing"
 