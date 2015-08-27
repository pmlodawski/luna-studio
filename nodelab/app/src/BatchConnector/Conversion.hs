{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module BatchConnector.Conversion where

import           Utils.PreludePlus
import           Text.ProtocolBuffers
import           Text.ProtocolBuffers.WireMessage
import           Text.ProtocolBuffers.Basic       (uToString)
import           Data.Sequence                    as Seq

import           Batch.Project
import           Batch.Library

import qualified Generated.Proto.Project.Project        as ProtoProject
import qualified Generated.Proto.Dep.Library.Library    as ProtoLibrary
import qualified Generated.Proto.Dep.Library.LibManager as ProtoLibManager

class ProtoSerializable m n | m -> n where
    decode :: m -> Maybe n

instance (ProtoSerializable m n) => ProtoSerializable (Seq m) [n] where
    decode = sequence . (fmap decode) . toList

instance ProtoSerializable ProtoProject.Project Project where
    decode proj = Project name path id <$> libs where
        name = fmap uToString $ ProtoProject.name proj
        path = uToString      $ ProtoProject.path proj
        id   = ProtoProject.id proj
        libs = decode $ ProtoLibManager.libraries $ ProtoProject.libManager proj

instance ProtoSerializable ProtoLibrary.Library Library where
    decode lib = Library <$> name <*> path where
        name = uToString <$> ProtoLibrary.name lib
        path = uToString <$> ProtoLibrary.path lib
