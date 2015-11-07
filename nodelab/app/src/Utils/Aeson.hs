module Utils.Aeson where

import           Utils.PreludePlus hiding ((.=))
import           Data.Aeson (ToJSON, toJSON, object, (.=), Value)
import qualified Data.HashMap.Strict as H
import           Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Text as Text

intMapToJSON :: ToJSON a => IntMap a -> Value
intMapToJSON map = object $ (\(k, v) -> (Text.pack $ show k) .= (toJSON v)) <$> IntMap.toList map
