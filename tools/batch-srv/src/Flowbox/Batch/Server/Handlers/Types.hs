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
    newTypeName,
    newTypeTuple,
) 
where

import           Data.Text.Lazy                                        (Text, unpack)
import qualified Data.Vector                                         as Vector
import           Data.Vector                                           (Vector)

import           Flowbox.Prelude                                       
import           Flowbox.Batch.Server.Handlers.Common                  (tRunScript)
import           Flowbox.Control.Error                                 
import           Flowbox.Luna.Tools.Serialize.Thrift.Conversion.Defs   ()
import qualified Flowbox.Luna.XOLD.Type.Type                         as Type
import           Flowbox.System.Log.Logger                             
import           Flowbox.Tools.Conversion                              
import qualified Types_Types                                         as TTypes



loggerIO :: LoggerIO
loggerIO = getLoggerIO "Flowbox.Batch.Server.Handlers.Types"


------ public api -------------------------------------------------

newTypeModule :: b -> Maybe Text -> Maybe (Vector TTypes.Type) -> IO TTypes.Type
newTypeModule _ mtname mtfields = tRunScript $ do 
    scriptIO $ loggerIO info "called newTypeModule"
    tname   <- mtname   <??> "'name' argument is missing"
    tfields <- mtfields <??> "'fields' argument is missing"
    fields  <- tryRight $ decode $ Vector.toList tfields 
    return $ encode $ Type.Module (unpack tname) fields


newTypeClass :: b -> Maybe Text -> Maybe (Vector Text) -> Maybe (Vector TTypes.Type) -> IO TTypes.Type
newTypeClass _ mtname mtparams mtfields = tRunScript $ do 
    scriptIO $ loggerIO info "called newTypeClass"
    tname      <- mtname   <??> "'name' argument is missing"
    tparams    <- mtparams <??> "'params' argument is missing"
    tfields    <- mtfields <??> "'fields' argument is missing"
    fields     <- tryRight $ decode $ Vector.toList tfields 
    let name   = unpack tname
        params = map (unpack) $ Vector.toList tparams
    return $ encode $ Type.Class name params fields


newTypeFunction :: b -> Maybe Text -> Maybe TTypes.Type -> Maybe TTypes.Type -> IO TTypes.Type
newTypeFunction _ mtname mtinputs mtoutputs = tRunScript $ do 
    scriptIO $ loggerIO info "called newTypeFunction"
    tname    <- mtname    <??> "'name' argument is missing"
    tinputs  <- mtinputs  <??> "'inputs' argument is missing"
    inputs   <- tryRight   $ decode tinputs
    toutputs <- mtoutputs <??> "'outputs' argument is missing"
    outputs  <- tryRight   $ decode toutputs
    return $ encode $ Type.Function (unpack tname) inputs outputs


newTypeUdefined :: b -> IO TTypes.Type
newTypeUdefined _ = do
    loggerIO info "called newTypeUdefined"
    return $ encode Type.Undefined


newTypeNamed :: b -> Maybe Text -> Maybe TTypes.Type -> IO TTypes.Type
newTypeNamed _ mtname mttype = tRunScript $ do 
    scriptIO $ loggerIO info "called newTypeNamed"
    tname <- mtname  <??> "'name' argument is missing"
    ttype <- mttype  <??> "'type' argument is missing"
    atype <- tryRight $ decode ttype 
    return $ encode $ Type.Named (unpack tname) atype


newTypeName :: b -> Maybe Text -> IO TTypes.Type
newTypeName _ mtname = tRunScript $ do 
    scriptIO $ loggerIO info "called newTypeName"
    tname <- mtname  <??> "'name' argument is missing"
    return $ encode $ Type.TypeName $ unpack tname


newTypeTuple :: b -> Maybe (Vector TTypes.Type) -> IO TTypes.Type
newTypeTuple _ mttypes = tRunScript $ do
    scriptIO $ loggerIO info "called newTypeTuple"
    ttypes <- mttypes <??> "'types' argument is missing"
    types  <- tryRight $ decode $ Vector.toList ttypes
    return $ encode $ Type.Tuple types
    
 