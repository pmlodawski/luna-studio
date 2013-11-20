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

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Pat where

import           Control.Applicative                                  
import qualified Data.Map                                           as Map
import qualified Text.ProtocolBuffers.Extensions                    as Extensions

import           Flowbox.Prelude                                      
import           Flowbox.Control.Error                                
import qualified Flowbox.Luna.Data.AST.Pat                          as Pat
import           Flowbox.Luna.Data.AST.Pat                            (Pat)
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic       
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Lit    ()
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Type   ()
import qualified Generated.Proto.Pat.App                            as GenApp
import qualified Generated.Proto.Pat.Con                            as GenCon
import qualified Generated.Proto.Pat.Lit                            as GenLit
import qualified Generated.Proto.Pat.Pat                            as Gen
import qualified Generated.Proto.Pat.Pat.Cls                        as GenCls
import qualified Generated.Proto.Pat.Tuple                          as GenTuple
import qualified Generated.Proto.Pat.Typed                          as GenTyped
import qualified Generated.Proto.Pat.Var                            as GenVar
import qualified Generated.Proto.Pat.Wildcard                       as GenWildcard



genPat :: GenCls.Cls -> Extensions.Key Maybe Gen.Pat v -> v -> Gen.Pat
genPat cls key ext = Extensions.putExt key (Just ext)
                   $ Gen.Pat cls $ Extensions.ExtField Map.empty


instance Convert Pat Gen.Pat where
    encode p = case p of 
        Pat.Var   i name     -> genPat GenCls.Var   GenVar.ext   $ GenVar.Var     (encodePJ i) (encodePJ name)
        Pat.Lit   i value    -> genPat GenCls.Lit   GenLit.ext   $ GenLit.Lit     (encodePJ i) (encodeJ value)
        Pat.Tuple i items    -> genPat GenCls.Tuple GenTuple.ext $ GenTuple.Tuple (encodePJ i) (encodeList items)
        Pat.Con   i name     -> genPat GenCls.Con   GenCon.ext   $ GenCon.Con     (encodePJ i) (encodePJ name)
        Pat.App   i src args -> genPat GenCls.App   GenApp.ext   $ GenApp.App     (encodePJ i) (encodeJ src) (encodeList args)
        Pat.Typed i pat cls  -> genPat GenCls.Typed GenTyped.ext $ GenTyped.Typed (encodePJ i) (encodeJ pat) (encodeJ cls)
        Pat.Wildcard i       -> genPat GenCls.Wildcard GenWildcard.ext $ GenWildcard.Wildcard (encodePJ i)
    decode p@(Gen.Pat cls _) = case cls of 
       GenCls.Var      -> do ext <- Extensions.getExt GenVar.ext p
                             (GenVar.Var mtid mtname) <- ext <?> "Failed to decode Pat.Var: extension is missing"
                             tid   <- mtid   <?> "Failed to decode Pat.Var: 'id' field is missing"
                             tname <- mtname <?> "Failed to decode Pat.Var: 'name' field is missing"
                             pure $ Pat.Var (decodeP tid) (decodeP tname)
       GenCls.Lit      -> do ext <- Extensions.getExt GenLit.ext p
                             (GenLit.Lit mtid mtvalue) <- ext <?> "Failed to decode Pat.Lit: extension is missing"
                             tid    <- mtid    <?> "Failed to decode Pat.Lit: 'id' field is missing"
                             tvalue <- mtvalue <?> "Failed to decode Pat.Lit: 'value' field is missing"
                             Pat.Lit (decodeP tid) <$> (decode tvalue)
       GenCls.Tuple    -> do ext <- Extensions.getExt GenTuple.ext p
                             (GenTuple.Tuple mtid titems) <- ext <?> "Failed to decode Pat.Tuple: extension is missing"
                             tid    <- mtid    <?> "Failed to decode Pat.Tuple: 'id' field is missing"
                             Pat.Tuple (decodeP tid) <$> (decodeList titems)
       GenCls.Con      -> do ext <- Extensions.getExt GenCon.ext p
                             (GenCon.Con mtid mtname) <- ext <?> "Failed to decode Pat.Con: extension is missing"
                             tid   <- mtid   <?> "Failed to decode Pat.Con: 'id' field is missing"
                             tname <- mtname <?> "Failed to decode Pat.Con: 'name' field is missing"
                             pure $ Pat.Con (decodeP tid) (decodeP tname)
       GenCls.App      -> do ext <- Extensions.getExt GenApp.ext p
                             (GenApp.App mtid mtsrc targs) <- ext <?> "Failed to decode Pat.App: extension is missing"
                             tid  <- mtid  <?> "Failed to decode Pat.App: 'id' field is missing"
                             tsrc <- mtsrc <?> "Failed to decode Pat.App: 'src' field is missing"
                             Pat.App (decodeP tid) <$> (decode tsrc) <*> decodeList targs
       GenCls.Typed    -> do ext <- Extensions.getExt GenTyped.ext p
                             (GenTyped.Typed mtid mtpat mttype) <- ext <?> "Failed to decode Pat.Typed: extension is missing"
                             tid   <- mtid   <?> "Failed to decode Pat.Typed: 'id' field is missing"
                             tpat  <- mtpat  <?> "Failed to decode Pat.Typed: 'pat' field is missing"
                             ttype <- mttype <?> "Failed to decode Pat.Typed: 'type' field is missing"
                             Pat.Typed (decodeP tid) <$> (decode tpat) <*> decode ttype
       GenCls.Wildcard -> do ext <- Extensions.getExt GenWildcard.ext p
                             (GenWildcard.Wildcard mtid) <- ext <?> "Failed to decode Pat.Wildcard: extension is missing"
                             tid   <- mtid   <?> "Failed to decode Pat.Wildcard: 'id' field is missing"
                             pure $ Pat.Wildcard (decodeP tid)
