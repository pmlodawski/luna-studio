---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Type where

import           Control.Applicative
import qualified Data.Map                                       as Map
import           Flowbox.Control.Error
import qualified Flowbox.Luna.Data.AST.Common                   as AST
import           Flowbox.Luna.Data.AST.Type                     (Type)
import qualified Flowbox.Luna.Data.AST.Type                     as Type
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Type.App                       as GenApp
import qualified Generated.Proto.Type.Con_                      as GenCon_
import qualified Generated.Proto.Type.Data                      as GenData
import qualified Generated.Proto.Type.Lambda                    as GenLambda
import qualified Generated.Proto.Type.Module                    as GenModule
import qualified Generated.Proto.Type.Tuple                     as GenTuple
import qualified Generated.Proto.Type.Type                      as Gen
import qualified Generated.Proto.Type.Type.Cls                  as GenCls
import qualified Generated.Proto.Type.Unknown                   as GenUnknown
import qualified Generated.Proto.Type.Var                       as GenVar
import qualified Text.ProtocolBuffers.Extensions                as Extensions



instance Convert Type Gen.Type where
    encode t = case t of
        Type.Unknown i               -> genType GenCls.Unknown i GenUnknown.ext $ GenUnknown.Unknown
        Type.Var     i name          -> genType GenCls.Var     i GenVar.ext     $ GenVar.Var         (Just $ encodeP name)
        Type.Tuple   i items         -> genType GenCls.Tuple   i GenTuple.ext   $ GenTuple.Tuple     (encodeList items)
        Type.Data    i name params   -> genType GenCls.Data    i GenData.ext    $ GenData.Data     (Just $ encodeP name) (encodeListP params)
        Type.Module  i path          -> genType GenCls.Module  i GenModule.ext  $ GenModule.Module   (encodeListP path)
        Type.Lambda  i inputs output -> genType GenCls.Lambda  i GenLambda.ext  $ GenLambda.Lambda   (encodeList inputs) (Just $ encode output)
        Type.Con     i segments      -> genType GenCls.Con_    i GenCon_.ext    $ GenCon_.Con_       (encodeListP segments)
        Type.App     i src args      -> genType GenCls.App     i GenApp.ext     $ GenApp.App         (Just $ encode src) (encodeList args)
        where
            genType :: GenCls.Cls -> AST.ID -> Extensions.Key Maybe Gen.Type v -> v -> Gen.Type
            genType cls i key ext = Extensions.putExt key (Just ext)
                                  $ Gen.Type cls (encodePJ i) $ Extensions.ExtField Map.empty

    decode t@(Gen.Type cls mtid _) = do
        i <- decodeP <$> mtid <?> "Failed to decode Type: 'id' field is missing"
        case cls of
            GenCls.Unknown -> do ext <- getExt GenUnknown.ext
                                 GenUnknown.Unknown <- ext <?> "Failed to decode Type.Unknown: extension is missing"
                                 pure $ Type.Unknown i
            GenCls.Var     -> do ext <- getExt GenVar.ext
                                 (GenVar.Var mtname) <- ext <?> "Failed to decode Type.Var: extension is missing"
                                 tname <- mtname <?> "Failed to decode Type.Var: 'name' field is missing"
                                 pure $ Type.Var i (decodeP tname)
            GenCls.Tuple   -> do ext <- getExt GenTuple.ext
                                 (GenTuple.Tuple titems) <- ext <?> "Failed to decode Type.Tuple: extension is missing"
                                 Type.Tuple i <$> decodeList titems
            GenCls.Data    -> do ext <- getExt GenData.ext
                                 (GenData.Data mtname tparams) <- ext <?> "Failed to decode Type.Data: extension is missing"
                                 tname <- mtname <?> "Failed to decode Type.Data: 'name' field is missing"
                                 pure $ Type.Data i (decodeP tname) (decodeListP tparams)
            GenCls.Module  -> do ext <- getExt GenModule.ext
                                 (GenModule.Module tpath) <- ext <?> "Failed to decode Type.Module: extension is missing"
                                 pure $ Type.Module i (decodeListP tpath)
            GenCls.Lambda  -> do ext <- getExt GenLambda.ext
                                 (GenLambda.Lambda tinputs mtoutput) <- ext <?> "Failed to decode Type.Lambda: extension is missing"
                                 toutput <- mtoutput <?> "Failed to decode Type.Lambda: 'output' field is missing"
                                 Type.Lambda i <$> (decodeList tinputs) <*> decode toutput
            GenCls.Con_    -> do ext <- getExt GenCon_.ext
                                 (GenCon_.Con_ tsegments) <- ext <?> "Failed to decode Type.Con: extension is missing"
                                 pure $ Type.Con i (decodeListP tsegments)
            GenCls.App     -> do ext <- getExt GenApp.ext
                                 (GenApp.App mtsrc targs) <- ext <?> "Failed to decode Type.App: extension is missing"
                                 tsrc <- mtsrc <?> "Failed to decode Type.App: 'src' field is missing"
                                 Type.App i <$> (decode tsrc) <*> (decodeList targs)
        where getExt k = case Extensions.getExt k t of
                                Right a -> return a
                                Left m  -> fail m

