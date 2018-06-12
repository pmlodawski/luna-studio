module LunaStudio.Data.PortRef
    ( module LunaStudio.Data.PortRef
    , nodeLoc
    ) where

import Control.Lens (makePrisms)
import           Control.DeepSeq         (NFData)
import           Data.Aeson.Types        (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.Binary             (Binary)
import qualified Data.Vector.Storable.Foreign as Foreign
import           Foreign.Ptr             (castPtr, plusPtr)
import           Foreign.Storable        (Storable(..))
import           LunaStudio.Data.Node    (NodeId)
import           LunaStudio.Data.NodeLoc (HasNodeLoc (..), NodeLoc)
import qualified LunaStudio.Data.NodeLoc as NodeLoc
import           LunaStudio.Data.Port    (AnyPortId (..), InPortId, OutPortId, OutPortIndex(Projection))
import           System.IO.Unsafe        (unsafePerformIO)
import           Prologue


data InPortRef  = InPortRef  { _dstNodeLoc :: NodeLoc
                             , _dstPortId :: InPortId
                             } deriving (Eq, Generic, Ord, Show)

data OutPortRefTemplate a b = OutPortRef { _srcNodeLoc :: a
                                         , _srcPortId  :: b
                                         } deriving (Eq, Generic, Ord, Show)

type OutPortRef = OutPortRefTemplate NodeLoc OutPortId
type OutPortRefS = OutPortRefTemplate NodeId (Foreign.Vector Int)


data AnyPortRef = OutPortRef' OutPortRef | InPortRef' InPortRef deriving (Eq, Generic, Show)

makeLenses ''AnyPortRef
makePrisms ''AnyPortRef
makeLenses ''OutPortRefTemplate
makePrisms ''OutPortRefTemplate
makeLenses ''InPortRef
makePrisms ''InPortRef

instance Binary      AnyPortRef
instance NFData      AnyPortRef
instance FromJSON    AnyPortRef
instance FromJSONKey AnyPortRef
instance ToJSON      AnyPortRef
instance ToJSONKey   AnyPortRef
instance Binary      InPortRef
instance NFData      InPortRef
instance FromJSON    InPortRef
instance FromJSONKey InPortRef
instance ToJSON      InPortRef
instance ToJSONKey   InPortRef
instance Binary      OutPortRef
instance NFData      OutPortRef
instance FromJSON    OutPortRef
instance ToJSON      OutPortRef


instance Ord AnyPortRef where
  (InPortRef'  _)  `compare` (OutPortRef' _) = LT
  (OutPortRef' _)  `compare` (InPortRef'  _) = GT
  (InPortRef'  a)  `compare` (InPortRef'  b) = a `compare` b
  (OutPortRef' a)  `compare` (OutPortRef' b) = a `compare` b

instance HasNodeLoc InPortRef  where nodeLoc = dstNodeLoc
instance HasNodeLoc OutPortRef where nodeLoc = srcNodeLoc
instance HasNodeLoc AnyPortRef where
    nodeLoc = lens getNodeLoc setNodeLoc  where
        getNodeLoc (OutPortRef' outPortRef) = outPortRef ^. nodeLoc
        getNodeLoc (InPortRef'  inPortRef)  = inPortRef  ^. nodeLoc
        setNodeLoc (OutPortRef' outPortRef) nl = OutPortRef' $ outPortRef & nodeLoc .~ nl
        setNodeLoc (InPortRef'  inPortRef ) nl = InPortRef'  $ inPortRef  & nodeLoc .~ nl

class    PortId a         where toAnyPortRef :: NodeLoc -> a -> AnyPortRef
instance PortId InPortId  where toAnyPortRef = InPortRef'  .: InPortRef
instance PortId OutPortId where toAnyPortRef = OutPortRef' .: OutPortRef
instance PortId AnyPortId where toAnyPortRef nl (InPortId'  pid) = toAnyPortRef nl pid
                                toAnyPortRef nl (OutPortId' pid) = toAnyPortRef nl pid

{-# DEPRECATED nodeId "Use nodeLoc" #-}
nodeId :: Lens' AnyPortRef NodeId
nodeId = nodeLoc . NodeLoc.nodeId

portId :: Lens' AnyPortRef AnyPortId
portId f (OutPortRef' (OutPortRef nl pid)) = OutPortRef' . OutPortRef nl . outPortId' <$> f (OutPortId' pid)
portId f (InPortRef'  (InPortRef  nl pid)) = InPortRef'  . InPortRef  nl . inPortId'  <$> f (InPortId'  pid)

dstNodeId :: Lens' InPortRef NodeId
dstNodeId = dstNodeLoc . NodeLoc.nodeId

srcNodeId :: Lens' OutPortRef NodeId
srcNodeId = srcNodeLoc . NodeLoc.nodeId

toPortRefS :: OutPortRef -> OutPortRefS
toPortRefS (OutPortRef a b) = OutPortRef (convert a) (unsafePerformIO $ Foreign.fromList (coerce b :: [Int]))

toPortRef :: OutPortRefS -> OutPortRef
toPortRef (OutPortRef a b) = OutPortRef (convert a) (coerce (unsafePerformIO (Foreign.toList b)) :: OutPortId)

instance (Storable a, Storable b) => Storable (OutPortRefTemplate a b) where
    sizeOf _ = sizeOf (undefined :: a) + sizeOf (undefined :: b)
    alignment _ = alignment (undefined :: Int)
    peek p = OutPortRef <$> peek (castPtr p)
                        <*> peek (p `plusPtr` sizeOf (undefined :: a))
    poke p op = do
        poke (castPtr p) (op ^. srcNodeLoc)
        poke (p `plusPtr` sizeOf (undefined :: a)) (op ^. srcPortId)

