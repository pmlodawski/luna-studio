{-# LANGUAGE FunctionalDependencies #-}

module BatchConnector.Conversion where

import           Utils.PreludePlus
import           Text.ProtocolBuffers
import           Text.ProtocolBuffers.WireMessage
import           Text.ProtocolBuffers.Basic       (uToString)

import           Batch.Project

import qualified Generated.Proto.Project.Project as ProtoProject

class (Wire m, ReflectDescriptor m) => ProtoSerializable m n | m -> n where
    deserialize :: m -> n

instance ProtoSerializable ProtoProject.Project Project where
    deserialize proj = Project (fmap uToString $ ProtoProject.name proj)
                               (uToString      $ ProtoProject.path proj)
                               (ProtoProject.id proj)
