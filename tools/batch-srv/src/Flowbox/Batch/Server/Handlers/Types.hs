---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of mthis file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
module Flowbox.Batch.Server.Handlers.Types (
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

import           Data.Text.Lazy                                        (Text, unpack)
import qualified Data.Vector                                         as Vector
import           Data.Vector                                           (Vector)

import           Flowbox.Batch.Server.Handlers.Common                  (tRunScript)
import qualified Types_Types                                         as TTypes
import           Flowbox.Control.Error                                 
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs   ()
import           Flowbox.Luna.XOLD.Type.Type                           (Type(..))
import           Flowbox.System.Log.Logger                             
import           Flowbox.Tools.Conversion                              



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Types"


------ public api -------------------------------------------------

newTypeModule :: b -> Maybe Text -> IO TTypes.Type
newTypeModule _ mtname = tRunScript $ do 
    scriptIO $ loggerIO info "called newTypeModule"
    tname <- mtname <??> "'name' argument is missing"
    return $ encode $ Module $ unpack tname


newTypeClass :: b -> Maybe Text -> Maybe (Vector Text) -> Maybe (Vector TTypes.Type) -> IO TTypes.Type
newTypeClass _ mtname mttypeparams mtparams = tRunScript $ do 
    scriptIO $ loggerIO info "called newTypeClass"
    tname       <- mtname       <??> "'name' argument is missing"
    ttypeparams <- mttypeparams <??> "'typeparams' argument is missing"
    tparams     <- mtparams     <??> "'params' argument is missing"
    aparams     <- tryRight $ decode $ Vector.toList tparams 
    let aname       = unpack tname
        atypeparams = map (unpack) $ Vector.toList ttypeparams
    return $ encode $ Class aname atypeparams aparams


newTypeFunction :: b -> Maybe Text -> Maybe TTypes.Type -> Maybe TTypes.Type -> IO TTypes.Type
newTypeFunction _ mtname mtinputs mtoutputs = tRunScript $ do 
    scriptIO $ loggerIO info "called newTypeFunction"
    tname    <- mtname    <??> "'name' argument is missing"
    tinputs  <- mtinputs  <??> "'inputs' argument is missing"
    ainputs  <- tryRight   $ decode tinputs
    toutputs <- mtoutputs <??> "'outputs' argument is missing"
    aoutputs <- tryRight   $ decode toutputs
    return $ encode $ Function (unpack tname) ainputs aoutputs


newTypeUdefined :: b -> IO TTypes.Type
newTypeUdefined _ = do
    loggerIO info "called newTypeUdefined"
    return $ encode Undefined


newTypeNamed :: b -> Maybe Text -> Maybe TTypes.Type -> IO TTypes.Type
newTypeNamed _ mtname mttype = tRunScript $ do 
    scriptIO $ loggerIO info "called newTypeNamed"
    tname <- mtname  <??> "'name' argument is missing"
    ttype <- mttype  <??> "'type' argument is missing"
    atype <- tryRight $ decode ttype 
    return $ encode $ Named (unpack tname) atype


newTypeVariable :: b -> Maybe Text -> IO TTypes.Type
newTypeVariable _ mtname = tRunScript $ do 
    scriptIO $ loggerIO info "called newTypeVariable"
    tname <- mtname  <??> "'name' argument is missing"
    return $ encode $ TypeVariable $ unpack tname


newTypeList :: b -> Maybe TTypes.Type -> IO TTypes.Type
newTypeList _ mttype = tRunScript $ do 
    scriptIO $ loggerIO info "called newTypeList"
    ttype <- mttype  <??> "'type' argument is missing"
    atype <- tryRight $ decode ttype 
    return $ encode $ List atype


newTypeTuple :: b -> Maybe (Vector TTypes.Type) -> IO TTypes.Type
newTypeTuple _ mttypes = tRunScript $ do
    scriptIO $ loggerIO info "called newTypeTuple"
    ttypes <- mttypes <??> "'types' argument is missing"
    atypes <- tryRight $ decode $ Vector.toList ttypes
    return $ encode $ Tuple atypes
    
 