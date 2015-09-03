{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module BatchConnector.Conversion where

import           Utils.PreludePlus
import           Text.ProtocolBuffers
import           Text.ProtocolBuffers.WireMessage
import           GHC.Float                        (float2Double, double2Float)
import           Text.ProtocolBuffers.Basic       (uToString, uFromString, Utf8(..))
import qualified Data.Sequence                    as Seq
import           Data.Text.Lazy.Encoding          (encodeUtf8, decodeUtf8)
import           Utils.Vector                     (Vector2(..), x, y)

import           Batch.Project                    as Project
import           Batch.Library                    as Library
import           Batch.Breadcrumbs
import           Object.Node

import qualified Generated.Proto.Project.Project           as ProtoProject
import qualified Generated.Proto.Dep.Library.Library       as ProtoLibrary
import qualified Generated.Proto.Dep.Library.LibManager    as ProtoLibManager
import qualified Generated.Proto.Dep.Crumb.Breadcrumbs     as ProtoBreadcrumbs

import qualified Generated.Proto.Dep.Graph.Node            as ProtoNode
import qualified Generated.Proto.Dep.Graph.NodeExpr        as ProtoExpr
import qualified Generated.Proto.Dep.Graph.Node.Cls        as NodeCls
import qualified Generated.Proto.Dep.Graph.NodeExpr.Cls    as ExprCls

import           Generated.Proto.Dep.Attributes.Attributes
import           Generated.Proto.Dep.Version.Version

class ProtoSerializable m n | m -> n, n -> m where
    decode :: m -> Maybe n
    encode :: n -> m

instance (ProtoSerializable m n) => ProtoSerializable (Seq m) [n] where
    decode = sequence . (fmap decode) . toList
    encode = Seq.fromList . (fmap encode)

instance ProtoSerializable ProtoProject.Project Project where
    decode proj = Project name path id <$> libs where
        name = fmap uToString $ ProtoProject.name proj
        path = uToString      $ ProtoProject.path proj
        id   = ProtoProject.id proj
        libs = decode $ ProtoLibManager.libraries $ ProtoProject.libManager proj
    encode proj = ProtoProject.Project (fmap uFromString $ proj ^. Project.name)
                                       (uFromString $ proj ^. Project.path)
                                       Seq.empty
                                       (ProtoLibManager.LibManager $ encode $ proj ^. Project.libs)
                                       (Attributes Seq.empty)
                                       (proj ^. Project.id)

instance ProtoSerializable ProtoLibrary.Library Library where
    decode lib = Library <$> name <*> path <*> id where
        name = uToString <$> ProtoLibrary.name lib
        path = uToString <$> ProtoLibrary.path lib
        id   = ProtoLibrary.id lib
    encode lib = ProtoLibrary.Library (Just $ lib ^. Library.id)
                                      (Just $ uFromString $ lib ^. Library.name)
                                      (Just $ Version Seq.empty Seq.empty)
                                      (Just $ uFromString $ lib ^. Library.path)
                                      Nothing
                                      Nothing
                                      (lib ^. Library.id)

instance ProtoSerializable ProtoBreadcrumbs.Breadcrumbs Breadcrumbs where
    decode crumbs = Just $ Breadcrumbs crumbs
    encode        = unBreadcrumbs

instance ProtoSerializable Utf8 Text where
    decode = Just . decodeUtf8 . utf8
    encode = Utf8 . encodeUtf8

instance ProtoSerializable ProtoNode.Node Node where
    decode node = Node <$> id <*> pure False <*> nodePos <*> expr <*> pure ports where
        id      = fromIntegral <$> ProtoNode.id node
        nodePos = Vector2 <$> (float2Double <$> ProtoNode.x node)
                          <*> (float2Double <$> ProtoNode.y node)
        expr    = (ProtoNode.expr node) >>= ProtoExpr.str >>= decode
        ports   = createPorts 1 1
    encode node = ProtoNode.Node NodeCls.Expr
                                 (Just $ fromIntegral $ node ^. nodeId)
                                 (Just expr)
                                 Nothing
                                 (Just $ double2Float $ node ^. nodePos . x)
                                 (Just $ double2Float $ node ^. nodePos . y)
        where
            expr       = ProtoExpr.NodeExpr ExprCls.String (Just $ encodedStr) Nothing
            encodedStr = encode $ node ^. expression
