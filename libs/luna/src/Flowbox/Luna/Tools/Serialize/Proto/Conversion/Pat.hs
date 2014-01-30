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

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Pat where

import           Control.Applicative
import qualified Data.Map                        as Map
import qualified Text.ProtocolBuffers.Extensions as Extensions

import           Flowbox.Control.Error
import qualified Flowbox.Luna.Data.AST.Common                       as AST
import           Flowbox.Luna.Data.AST.Pat                          (Pat)
import qualified Flowbox.Luna.Data.AST.Pat                          as Pat
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Lit  ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Type ()
import           Flowbox.Prelude
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic
import qualified Generated.Proto.Pat.App                            as GenApp
import qualified Generated.Proto.Pat.Con_                           as GenCon_
import qualified Generated.Proto.Pat.Lit                            as GenLit
import qualified Generated.Proto.Pat.Pat                            as Gen
import qualified Generated.Proto.Pat.Pat.Cls                        as GenCls
import qualified Generated.Proto.Pat.Tuple                          as GenTuple
import qualified Generated.Proto.Pat.Typed                          as GenTyped
import qualified Generated.Proto.Pat.Var                            as GenVar
import qualified Generated.Proto.Pat.Wildcard                       as GenWildcard



instance Convert Pat Gen.Pat where
    encode p = case p of
        Pat.Var   i name     -> genPat GenCls.Var   i GenVar.ext   $ GenVar.Var     (encodePJ name)
        Pat.Lit   i value    -> genPat GenCls.Lit   i GenLit.ext   $ GenLit.Lit     (encodeJ value)
        Pat.Tuple i items    -> genPat GenCls.Tuple i GenTuple.ext $ GenTuple.Tuple (encodeList items)
        Pat.Con   i name     -> genPat GenCls.Con_  i GenCon_.ext  $ GenCon_.Con_   (encodePJ name)
        Pat.App   i src args -> genPat GenCls.App   i GenApp.ext   $ GenApp.App     (encodeJ src) (encodeList args)
        Pat.Typed i pat cls  -> genPat GenCls.Typed i GenTyped.ext $ GenTyped.Typed (encodeJ pat) (encodeJ cls)
        Pat.Wildcard i       -> genPat GenCls.Wildcard i GenWildcard.ext $ GenWildcard.Wildcard
        where
            genPat :: GenCls.Cls -> AST.ID -> Extensions.Key Maybe Gen.Pat v -> v -> Gen.Pat
            genPat cls i key ext = Extensions.putExt key (Just ext)
                                 $ Gen.Pat cls (encodePJ i) $ Extensions.ExtField Map.empty

    decode p@(Gen.Pat cls mtid _) = do
        i <- decodeP <$> mtid <?> "Failed to decode Pat: 'id' field is missing"
        case cls of
           GenCls.Var      -> do ext <- getExt GenVar.ext
                                 (GenVar.Var mtname) <- ext <?> "Failed to decode Pat.Var: extension is missing"
                                 tname <- mtname <?> "Failed to decode Pat.Var: 'name' field is missing"
                                 pure $ Pat.Var i (decodeP tname)
           GenCls.Lit      -> do ext <- getExt GenLit.ext
                                 (GenLit.Lit mtvalue) <- ext <?> "Failed to decode Pat.Lit: extension is missing"
                                 tvalue <- mtvalue <?> "Failed to decode Pat.Lit: 'value' field is missing"
                                 Pat.Lit i <$> (decode tvalue)
           GenCls.Tuple    -> do ext <- getExt GenTuple.ext
                                 (GenTuple.Tuple titems) <- ext <?> "Failed to decode Pat.Tuple: extension is missing"
                                 Pat.Tuple i <$> (decodeList titems)
           GenCls.Con_     -> do ext <- getExt GenCon_.ext
                                 (GenCon_.Con_ mtname) <- ext <?> "Failed to decode Pat.Con: extension is missing"
                                 tname <- mtname <?> "Failed to decode Pat.Con: 'name' field is missing"
                                 pure $ Pat.Con i (decodeP tname)
           GenCls.App      -> do ext <- getExt GenApp.ext
                                 (GenApp.App mtsrc targs) <- ext <?> "Failed to decode Pat.App: extension is missing"
                                 tsrc <- mtsrc <?> "Failed to decode Pat.App: 'src' field is missing"
                                 Pat.App i <$> (decode tsrc) <*> decodeList targs
           GenCls.Typed    -> do ext <- getExt GenTyped.ext
                                 (GenTyped.Typed mtpat mttype) <- ext <?> "Failed to decode Pat.Typed: extension is missing"
                                 tpat  <- mtpat  <?> "Failed to decode Pat.Typed: 'pat' field is missing"
                                 ttype <- mttype <?> "Failed to decode Pat.Typed: 'type' field is missing"
                                 Pat.Typed i <$> (decode tpat) <*> decode ttype
           GenCls.Wildcard -> do ext <- getExt GenWildcard.ext
                                 GenWildcard.Wildcard <- ext <?> "Failed to decode Pat.Wildcard: extension is missing"
                                 pure $ Pat.Wildcard i
       where getExt k = case Extensions.getExt k p of
                                Right a -> return a
                                Left m  -> fail m
