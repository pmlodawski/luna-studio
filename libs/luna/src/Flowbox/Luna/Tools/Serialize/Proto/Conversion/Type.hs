---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Type where

import           Control.Applicative                              
import qualified Data.Map                                       as Map
import qualified Text.ProtocolBuffers.Extensions                as Extensions
import           Flowbox.Prelude                                  
import           Flowbox.Control.Error                            
import qualified Flowbox.Luna.Data.AST.Type                     as Type
import           Flowbox.Luna.Data.AST.Type                       (Type)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic   
import qualified Generated.Proto.Type.App                       as GenApp
import qualified Generated.Proto.Type.Class                     as GenClass
import qualified Generated.Proto.Type.Con                       as GenCon
import qualified Generated.Proto.Type.Lambda                    as GenLambda
import qualified Generated.Proto.Type.Module                    as GenModule
import qualified Generated.Proto.Type.Tuple                     as GenTuple
import qualified Generated.Proto.Type.Type                      as Gen
import qualified Generated.Proto.Type.Type.Cls                  as GenCls
import qualified Generated.Proto.Type.Unknown                   as GenUnknown
import qualified Generated.Proto.Type.Var                       as GenVar



instance Convert Type Gen.Type where
    encode t = case t of 
        Type.Unknown i               -> genType GenCls.Unknown GenUnknown.ext $ GenUnknown.Unknown (Just $ encodeP i)
        Type.Var     i name          -> genType GenCls.Var     GenVar.ext     $ GenVar.Var         (Just $ encodeP i) (Just $ encodeP name)
        Type.Tuple   i items         -> genType GenCls.Tuple   GenTuple.ext   $ GenTuple.Tuple     (Just $ encodeP i) (encodeList items)
        Type.Class   i name params   -> genType GenCls.Class   GenClass.ext   $ GenClass.Class     (Just $ encodeP i) (Just $ encodeP name) (encodeListP params)
        Type.Module  i path          -> genType GenCls.Module  GenModule.ext  $ GenModule.Module   (Just $ encodeP i) (encodeListP path)
        Type.Lambda  i inputs output -> genType GenCls.Lambda  GenLambda.ext  $ GenLambda.Lambda   (Just $ encodeP i) (encodeList inputs) (Just $ encode output)
        Type.Con     i segments      -> genType GenCls.Con     GenCon.ext     $ GenCon.Con         (Just $ encodeP i) (encodeListP segments)
        Type.App     i src args      -> genType GenCls.App     GenApp.ext     $ GenApp.App         (Just $ encodeP i) (Just $ encode src) (encodeList args)
        where
            genType :: GenCls.Cls -> Extensions.Key Maybe Gen.Type v -> v -> Gen.Type
            genType cls key ext = Extensions.putExt key (Just ext)
                                $ Gen.Type cls $ Extensions.ExtField Map.empty

    decode t@(Gen.Type cls _) = case cls of 
        GenCls.Unknown -> do ext <- getExt GenUnknown.ext
                             (GenUnknown.Unknown mtid) <- ext <?> "Failed to decode Type.Unknown: extension is missing"
                             tid <- mtid <?> "Failed to decode Type.Unknown: 'id' field is missing"
                             pure $ Type.Unknown (decodeP tid)
        GenCls.Var     -> do ext <- getExt GenVar.ext
                             (GenVar.Var mtid mtname) <- ext <?> "Failed to decode Type.Var: extension is missing"
                             tid   <- mtid   <?> "Failed to decode Type.Var: 'id' field is missing"
                             tname <- mtname <?> "Failed to decode Type.Var: 'name' field is missing"
                             pure $ Type.Var (decodeP tid) (decodeP tname)
        GenCls.Tuple   -> do ext <- getExt GenTuple.ext
                             (GenTuple.Tuple mtid titems) <- ext <?> "Failed to decode Type.Tuple: extension is missing"
                             tid <- mtid <?> "Failed to decode Type.Tuple: 'id' field is missing"
                             Type.Tuple (decodeP tid) <$> decodeList titems
        GenCls.Class   -> do ext <- getExt GenClass.ext
                             (GenClass.Class mtid mtname tparams) <- ext <?> "Failed to decode Type.Class: extension is missing"
                             tid   <- mtid   <?> "Failed to decode Type.Class: 'id' field is missing"
                             tname <- mtname <?> "Failed to decode Type.Class: 'name' field is missing"
                             pure $ Type.Class (decodeP tid) (decodeP tname) (decodeListP tparams)
        GenCls.Module  -> do ext <- getExt GenModule.ext
                             (GenModule.Module mtid tpath) <- ext <?> "Failed to decode Type.Module: extension is missing"
                             tid <- mtid <?> "Failed to decode Type.Module: 'id' field is missing"
                             pure $ Type.Module (decodeP tid) (decodeListP tpath)
        GenCls.Lambda  -> do ext <- getExt GenLambda.ext
                             (GenLambda.Lambda mtid tinputs mtoutput) <- ext <?> "Failed to decode Type.Lambda: extension is missing"
                             tid     <- mtid     <?> "Failed to decode Type.Lambda: 'id' field is missing"
                             toutput <- mtoutput <?> "Failed to decode Type.Lambda: 'output' field is missing"
                             Type.Lambda (decodeP tid) <$> (decodeList tinputs) <*> decode toutput
        GenCls.Con     -> do ext <- getExt GenCon.ext
                             (GenCon.Con mtid tsegments) <- ext <?> "Failed to decode Type.Con: extension is missing"
                             tid <- mtid <?> "Failed to decode Type.Con: 'id' field is missing"
                             pure $ Type.Con (decodeP tid) (decodeListP tsegments)
        GenCls.App     -> do ext <- getExt GenApp.ext
                             (GenApp.App mtid mtsrc targs) <- ext <?> "Failed to decode Type.App: extension is missing"
                             tid  <- mtid  <?> "Failed to decode Type.App: 'id' field is missing"
                             tsrc <- mtsrc <?> "Failed to decode Type.App: 'src' field is missing"
                             Type.App (decodeP tid) <$> (decode tsrc) <*> (decodeList targs)
        where getExt = flip Extensions.getExt t