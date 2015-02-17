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

module Luna.DEP.Data.Serialize.Proto.Conversion.Type where

import           Control.Applicative
import qualified Data.Map                          as Map
import           Flowbox.Control.Error
import           Flowbox.Data.Convert
import           Flowbox.Prelude
import qualified Generated.Proto.Dep.Type.App      as GenApp
import qualified Generated.Proto.Dep.Type.Con_     as GenCon_
import qualified Generated.Proto.Dep.Type.Data     as GenData
import qualified Generated.Proto.Dep.Type.Function as GenFunction
import qualified Generated.Proto.Dep.Type.List     as GenList
import qualified Generated.Proto.Dep.Type.Module   as GenModule
import qualified Generated.Proto.Dep.Type.Tuple    as GenTuple
import qualified Generated.Proto.Dep.Type.Type     as Gen
import qualified Generated.Proto.Dep.Type.Type.Cls as GenCls
import qualified Generated.Proto.Dep.Type.Unknown  as GenUnknown
import qualified Generated.Proto.Dep.Type.Var      as GenVar
import qualified Luna.DEP.AST.AST                  as AST
import           Luna.DEP.AST.Type                 (Type)
import qualified Luna.DEP.AST.Type                 as Type
import qualified Text.ProtocolBuffers.Extensions   as Extensions



instance Convert Type Gen.Type where
    encode t = case t of
        Type.Unknown  i               -> genType GenCls.Unknown  i GenUnknown.ext    GenUnknown.Unknown
        Type.Var      i name          -> genType GenCls.Var      i GenVar.ext      $ GenVar.Var           (encodePJ name)
        Type.Tuple    i items         -> genType GenCls.Tuple    i GenTuple.ext    $ GenTuple.Tuple       (encode items)
        Type.List     i item          -> genType GenCls.List     i GenList.ext     $ GenList.List         (encodeJ item)
        Type.Data     i name params   -> genType GenCls.Data     i GenData.ext     $ GenData.Data         (encodePJ name) (encodeP params)
        Type.Module   i name path     -> genType GenCls.Module   i GenModule.ext   $ GenModule.Module     (encodePJ name) (encodeP path)
        Type.Function i inputs output -> genType GenCls.Function i GenFunction.ext $ GenFunction.Function (encode inputs) (encodeJ output)
        Type.Con      i segments      -> genType GenCls.Con_     i GenCon_.ext     $ GenCon_.Con_         (encodeP segments)
        Type.App      i src args      -> genType GenCls.App      i GenApp.ext      $ GenApp.App           (encodeJ src) (encode args)
        where
            genType :: GenCls.Cls -> AST.ID -> Extensions.Key Maybe Gen.Type v -> v -> Gen.Type
            genType cls i key ext = Extensions.putExt key (Just ext)
                                  $ Gen.Type cls (encodePJ i) $ Extensions.ExtField Map.empty

    decode t@(Gen.Type cls mtid _) = do
        i <- decodeP <$> mtid <?> "Failed to decode Type: 'id' field is missing"
        case cls of
            GenCls.Unknown  -> do GenUnknown.Unknown <- getExt GenUnknown.ext "Type.Unknown"
                                  pure $ Type.Unknown i
            GenCls.Var      -> do GenVar.Var name <- getExt GenVar.ext "Type.Var"
                                  Type.Var i <$> decodePJ name (missing "Type.Var" "name")
            GenCls.Tuple    -> do GenTuple.Tuple items <- getExt GenTuple.ext "Type.Tuple"
                                  Type.Tuple i <$> decode items
            GenCls.List     -> do GenList.List item <- getExt GenList.ext "Type.List"
                                  Type.List i <$> decodeJ item (missing "Type.List" "item")
            GenCls.Data     -> do GenData.Data name params <- getExt GenData.ext "Type.Data"
                                  Type.Data i <$> decodePJ name (missing "Type.Data" "name")
                                              <*> pure (decodeP params)
            GenCls.Module   -> do GenModule.Module name path <- getExt GenModule.ext "Type.Module"
                                  Type.Module i <$> decodePJ name (missing "Type.Module" "name")
                                                <*> pure (decodeP path)
            GenCls.Function -> do GenFunction.Function inputs output <- getExt GenFunction.ext "Type.Function"
                                  Type.Function i <$> decode inputs <*> decodeJ output (missing "Type.Function" "output")
            GenCls.Con_     -> do GenCon_.Con_ segments <- getExt GenCon_.ext "Type.Con"
                                  pure $ Type.Con i (decodeP segments)
            GenCls.App      -> do GenApp.App src args <- getExt GenApp.ext "Type.App"
                                  Type.App i <$> decodeJ src (missing "Type.App" "src") <*> decode args
      where getExt key datatype = Extensions.getExt key t <?&> missing datatype "extension"

