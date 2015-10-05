{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module BatchConnector.Conversion where

import           Utils.PreludePlus
import           Text.ProtocolBuffers
import           Text.ProtocolBuffers.WireMessage
import           Text.ProtocolBuffers.Extensions  (putExt, getExt, Key, ExtField(..))
import           GHC.Float                        (float2Double, double2Float)
import           Text.ProtocolBuffers.Basic       (uToString, uFromString, Utf8(..))
import qualified Data.Sequence                    as Seq
import qualified Data.Map                         as Map
import           Data.Int
import           Data.Text.Lazy.Encoding          (encodeUtf8, decodeUtf8)
import           Utils.Vector                     (Vector2(..), x, y)
import qualified Utils.MockHelper  as MockHelper

import           Batch.Project     as Project
import           Batch.Library     as Library
import           Batch.Breadcrumbs
import           Batch.Value
import           Batch.RunStatus   hiding (nodeId)
import           Object.Node
import           Object.Object     (PortId(..), PortType(..))

import qualified Generated.Proto.Project.Project           as ProtoProject
import qualified Generated.Proto.Dep.Library.Library       as ProtoLibrary
import qualified Generated.Proto.Dep.Library.LibManager    as ProtoLibManager

import qualified Generated.Proto.Dep.Crumb.Breadcrumbs     as ProtoBreadcrumbs
import qualified Generated.Proto.Dep.Crumb.Crumb           as ProtoCrumb
import qualified Generated.Proto.Dep.Crumb.Crumb.Cls       as CrumbCls
import qualified Generated.Proto.Dep.Crumb.Module          as ModuleCrumb
import qualified Generated.Proto.Dep.Crumb.Function        as FunctionCrumb
import qualified Generated.Proto.Dep.Name.Name             as ProtoName

import qualified Generated.Proto.Dep.Graph.Node            as ProtoNode
import qualified Generated.Proto.Dep.Graph.NodeExpr        as ProtoExpr
import qualified Generated.Proto.Dep.Graph.Node.Cls        as NodeCls
import qualified Generated.Proto.Dep.Graph.NodeExpr.Cls    as ExprCls
import qualified Generated.Proto.Dep.Graphview.EdgeView    as ProtoEdge

import qualified Generated.Proto.Data.SValue               as SValue
import qualified Generated.Proto.Data.SValue.Type          as SValueType
import qualified Generated.Proto.Data.IntData              as IntData
import qualified Generated.Proto.Data.FloatData            as FloatData
import qualified Generated.Proto.Data.StringData           as StringData
import qualified Generated.Proto.Data.CharData             as CharData
import qualified Generated.Proto.Data.BoolData             as BoolData

import           Generated.Proto.Dep.Attributes.Attributes
import           Generated.Proto.Dep.Version.Version

import qualified Generated.Proto.Interpreter.ProfileInfo            as ProtoProfile
import qualified Generated.Proto.Interpreter.CallPoint              as ProtoCallPoint
import qualified Generated.Proto.Interpreter.CallPointPath          as ProtoCallPointPath
import qualified Generated.Proto.Interpreter.Interpreter.Run.Update as ProtoRunStatus

maybeGetExt :: Key Maybe msg ext -> msg -> Maybe ext
maybeGetExt key msg = case getExt key msg of
    Left  _   -> Nothing
    Right val -> val

class ProtoReadable m n | m -> n where
    decode :: m -> Maybe n

class ProtoWritable m n | n -> m where
    encode :: n -> m

instance ProtoReadable Utf8 Text where
    decode = Just . decodeUtf8 . utf8

instance ProtoWritable Utf8 Text where
    encode = Utf8 . encodeUtf8

instance ProtoReadable Int32 Int where
    decode = Just . fromIntegral

instance ProtoWritable Int32 Int where
    encode = fromIntegral

instance (ProtoReadable m n) => ProtoReadable (Seq m) [n] where
    decode = Just . catMaybes . (fmap decode) . toList

instance (ProtoWritable m n) => ProtoWritable (Seq m) [n] where
    encode = Seq.fromList . (fmap encode)

instance ProtoReadable ProtoProject.Project Project where
    decode proj = Project name path id <$> libs where
        name = fmap uToString $ ProtoProject.name proj
        path = uToString      $ ProtoProject.path proj
        id   = ProtoProject.id proj
        libs = decode $ ProtoLibManager.libraries $ ProtoProject.libManager proj

instance ProtoWritable ProtoProject.Project Project where
    encode proj = ProtoProject.Project (fmap uFromString $ proj ^. Project.name)
                                       (uFromString $ proj ^. Project.path)
                                       Seq.empty
                                       (ProtoLibManager.LibManager $ encode $ proj ^. Project.libs)
                                       (Attributes Seq.empty)
                                       (proj ^. Project.id)

instance ProtoReadable ProtoLibrary.Library Library where
    decode lib = Library <$> name <*> path <*> id where
        name = uToString <$> ProtoLibrary.name lib
        path = uToString <$> ProtoLibrary.path lib
        id   = ProtoLibrary.id lib

instance ProtoWritable ProtoLibrary.Library Library where
    encode lib = ProtoLibrary.Library (Just $ lib ^. Library.id)
                                      (Just $ uFromString $ lib ^. Library.name)
                                      (Just $ Version Seq.empty Seq.empty)
                                      (Just $ uFromString $ lib ^. Library.path)
                                      Nothing
                                      Nothing
                                      (lib ^. Library.id)

instance ProtoReadable ProtoCrumb.Crumb Crumb where
    decode crumb@(ProtoCrumb.Crumb cls _) = case cls of
        CrumbCls.Function -> do
            functionCrumb <- maybeGetExt FunctionCrumb.ext crumb
            name          <- FunctionCrumb.name functionCrumb
            baseName      <- ProtoName.base name
            return $ Function $ uToString baseName
        CrumbCls.Module -> do
            moduleCrumb <- maybeGetExt ModuleCrumb.ext crumb
            name        <- ModuleCrumb.name moduleCrumb
            return $ Module $ uToString name
        _ -> Nothing

instance ProtoWritable ProtoCrumb.Crumb Crumb where
    encode crumb = case crumb of
        Module name   -> makeCrumb CrumbCls.Module ModuleCrumb.ext $
                         Just $ ModuleCrumb.Module $ Just $ uFromString name
        Function name -> makeCrumb CrumbCls.Function FunctionCrumb.ext $
                         Just $ FunctionCrumb.Function (Just $ ProtoName.Name (Just $ uFromString name) Seq.empty)
                                                       Seq.empty
        where
            makeCrumb tpe key ext = putExt key ext $ ProtoCrumb.Crumb tpe $ ExtField Map.empty

instance ProtoReadable ProtoBreadcrumbs.Breadcrumbs Breadcrumbs where
    decode (ProtoBreadcrumbs.Breadcrumbs crumbs) = Breadcrumbs <$> decode crumbs

instance ProtoWritable ProtoBreadcrumbs.Breadcrumbs Breadcrumbs where
    encode (Breadcrumbs crumbs) = ProtoBreadcrumbs.Breadcrumbs $ encode crumbs

instance ProtoReadable ProtoNode.Node Node where
    decode node = Node <$> id <*> pure False <*> nodePos <*> expr <*> ports <*> nodeType where
        id       = fromIntegral <$> ProtoNode.id node
        nodePos  = Vector2 <$> (float2Double <$> ProtoNode.x node)
                          <*> (float2Double <$> ProtoNode.y node)
        expr     = (ProtoNode.expr node) >>= ProtoExpr.str >>= decode
        ports    = createPorts <$> expr
        nodeType = MockHelper.getNodeType <$> expr

instance ProtoWritable ProtoNode.Node Node where
    encode node = ProtoNode.Node NodeCls.Expr
                                 (Just $ fromIntegral $ node ^. nodeId)
                                 (Just expr)
                                 Nothing
                                 (Just $ double2Float $ node ^. nodePos . x)
                                 (Just $ double2Float $ node ^. nodePos . y)
        where
            expr       = ProtoExpr.NodeExpr ExprCls.String (Just $ encodedStr) Nothing
            encodedStr = encode $ node ^. expression

instance ProtoReadable ProtoEdge.EdgeView (PortRef, PortRef) where
    decode edge = do
        sourceNodeId      <- fromIntegral   <$> ProtoEdge.nodeSrc edge
        destinationNodeId <- fromIntegral   <$> ProtoEdge.nodeDst edge
        sourcePort        <- portIdFromList <$> (decode $ ProtoEdge.portSrc edge)
        destinationPort   <- portIdFromList <$> (decode $ ProtoEdge.portDst edge)
        return (PortRef sourceNodeId OutputPort sourcePort,
                PortRef destinationNodeId InputPort destinationPort)
        where
            portIdFromList []       = AllPorts
            portIdFromList (x : xs) = PortNum $ fromIntegral x

instance ProtoWritable ProtoEdge.EdgeView (PortRef, PortRef) where
    encode ((PortRef srcNode _ srcPort), (PortRef dstNode _ dstPort)) = ProtoEdge.EdgeView (Just $ fromIntegral srcNode)
                                                                                           (Just $ fromIntegral dstNode)
                                                                                           (encode $ portIdToList srcPort)
                                                                                           (encode $ portIdToList dstPort)
        where
            portIdToList AllPorts    = []
            portIdToList (PortNum x) = [x]

instance ProtoReadable SValue.SValue Value where
    decode msg@(SValue.SValue tpe _) = case tpe of
        SValueType.Int -> do
            intData   <- maybeGetExt IntData.data' msg
            let value =  IntData.svalue intData
            decoded   <- decode value
            return $ IntValue decoded
        SValueType.Float -> do
            floatData <- maybeGetExt FloatData.data' msg
            let value =  FloatData.svalue floatData
            return $ FloatValue value
        SValueType.String -> do
            stringData <- maybeGetExt StringData.data' msg
            let value  =  StringData.svalue stringData
            return $ StringValue $ uToString value
        SValueType.Char -> do
            charData  <- maybeGetExt CharData.data' msg
            let value =  CharData.svalue charData
            return $ CharValue $ chr $ fromIntegral value
        SValueType.Bool -> do
            boolData <- maybeGetExt BoolData.data' msg
            let value = BoolData.svalue boolData
            return $ BoolValue value
        _ -> Nothing

instance ProtoWritable SValue.SValue Value where
    encode value = case value of
        FloatValue val  -> makeSValue SValueType.Float FloatData.data' $ Just $ FloatData.FloatData $ val
        IntValue val    -> makeSValue SValueType.Int IntData.data' $ Just $ IntData.IntData $ encode val
        StringValue val -> makeSValue SValueType.String StringData.data' $ Just $ StringData.StringData $ uFromString val
        CharValue val   -> makeSValue SValueType.Char CharData.data' $ Just $ CharData.CharData $ fromIntegral $ ord val
        BoolValue val   -> makeSValue SValueType.Bool BoolData.data' $ Just $ BoolData.BoolData val
        where
            makeSValue tpe key ext = putExt key ext $ SValue.SValue tpe $ ExtField Map.empty


instance ProtoReadable ProtoProfile.ProfileInfo ProfileInfo where
    decode profileInfo = case decode $ ProtoProfile.callPointPath profileInfo of
        Just id -> Just $ ProfileInfo id
                                      (ProtoProfile.totalCpuTime  profileInfo)
                                      (ProtoProfile.totalRealTime profileInfo)
                                      (ProtoProfile.compileTime   profileInfo)
                                      (ProtoProfile.executeTime   profileInfo)
                                      (ProtoProfile.computeTime   profileInfo)
        Nothing -> Nothing

instance ProtoReadable ProtoCallPoint.CallPoint Int where
    decode callpoint = decode $ ProtoCallPoint.nodeID callpoint

instance ProtoReadable ProtoCallPointPath.CallPointPath Int where
    decode callpointPath = case (toList $ ProtoCallPointPath.calls callpointPath) of
        []              -> Nothing
        (callpoint : _) -> decode callpoint

instance ProtoReadable ProtoRunStatus.Update RunStatus where
    decode status = RunStatus <$> decode (ProtoRunStatus.profileInfos status)
