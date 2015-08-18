{-# LANGUAGE ExistentialQuantification #-}

module Object.Widget.Toggle where

import           Utils.PreludePlus
import           Utils.Vector
import           Data.Fixed
import           GHCJS.Types (JSRef)
import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

import           Object.Widget
import           Numeric


data Toggle = Toggle { _refId    :: Int
                     , _pos      :: Vector2 Double
                     , _size     :: Vector2 Double
                     , _label    :: Text
                     , _value    :: Bool
                     } deriving (Eq, Show, Typeable)

makeLenses ''Toggle

instance IsDisplayObject Toggle where
    objectId       b = b ^. refId
