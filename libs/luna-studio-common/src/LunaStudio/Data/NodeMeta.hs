{-# LANGUAGE TypeApplications #-}

module LunaStudio.Data.NodeMeta where

import           Data.Aeson.Types           (FromJSON, ToJSON)
import           Data.Binary                (Binary)
import qualified Data.Vector.Storable.Foreign as Foreign
import           Foreign.Ptr                (castPtr, plusPtr)
import           Foreign.Storable           (Storable(..))
import qualified Foreign.Storable           as Storable
import           Foreign.Storable.Tuple     ()
import           Foreign.Storable.Utils     (sizeOf')
import           LunaStudio.Data.Position   (Position)
import           LunaStudio.Data.Visualizer (VisualizerName, VisualizerPath)
import           Prologue
import           System.IO.Unsafe           (unsafePerformIO)

type VName = Foreign.Vector Char
type VPath = Foreign.Vector Char

data NodeMetaTemplate t = NodeMeta { _position           :: Position
                                   , _displayResult      :: Bool
                                   , _selectedVisualizer :: Maybe (t, t)
                                   } deriving (Eq, Generic, Show)

makeLenses ''NodeMetaTemplate

instance Eq a => Ord (NodeMetaTemplate a) where
    compare a b = compare (a ^. position) (b ^. position)

type NodeMeta = NodeMetaTemplate Text
type NodeMetaS = NodeMetaTemplate (Foreign.Vector Char)

instance Default (NodeMetaTemplate t) where
    def = NodeMeta def False def

instance Binary   NodeMeta
instance NFData   NodeMeta
instance FromJSON NodeMeta
instance ToJSON   NodeMeta

wordSize :: Int
wordSize = Storable.sizeOf @Int undefined

instance forall a. Storable a => Storable (Maybe a) where
    sizeOf    _ = sizeOf' @a + wordSize
    alignment _ = sizeOf' @Int
    peek ptr    = (Storable.peekByteOff ptr 0 :: IO Int) >>= \case
        0 -> pure Nothing
        1 -> Just <$> Storable.peekByteOff ptr wordSize
        a -> error $ "Storable.Maybe peek: unrecognized constructor: " <> show a <> " at " <> show ptr
    poke ptr (Just x) = Storable.pokeByteOff ptr 0 (1 :: Int) >> Storable.pokeByteOff ptr wordSize x
    poke ptr Nothing  = Storable.pokeByteOff ptr 0 (0 :: Int)


instance Storable NodeMetaS where
    sizeOf _  = sizeOf (undefined :: Position)
              + sizeOf (undefined :: Bool)
              + sizeOf (undefined :: Maybe (VName, VPath))
    alignment _ = 8
    peek ptr  = NodeMeta <$> (peek (castPtr ptr))
                         <*> (peek (ptr `plusPtr` sizeOf (undefined :: Position)))
                         <*> (peek (ptr `plusPtr` (sizeOf (undefined :: Position) + sizeOf (undefined :: Bool))))
    poke p nm = do
        poke (castPtr p) (nm ^. position)
        poke (p `plusPtr` sizeOf (undefined::Position)) (nm ^. displayResult)
        poke (p `plusPtr` (sizeOf (undefined::Position) + sizeOf (undefined::Bool))) (nm ^. selectedVisualizer)

toNodeMeta :: NodeMetaS -> NodeMeta
toNodeMeta (NodeMeta p d s) = NodeMeta p d (over both (convert . unsafePerformIO . Foreign.toList) <$> s)

toNodeMetaS :: NodeMeta -> NodeMetaS
toNodeMetaS (NodeMeta p d s) = NodeMeta p d (over both (unsafePerformIO . Foreign.fromList . convert) <$> s)