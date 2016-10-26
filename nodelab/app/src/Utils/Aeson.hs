module Utils.Aeson where

import           Data.Aeson        (ToJSON, Value, object, toJSON, (.=))
import           Data.IntMap.Lazy  (IntMap)
import qualified Data.IntMap.Lazy  as IntMap
import qualified Data.Text         as Text
import           Utils.PreludePlus hiding ((.=))

intMapToJSON :: ToJSON a => IntMap a -> Value
intMapToJSON intmap = object $ (\(k, v) -> (Text.pack $ show k) .= (toJSON v)) <$> IntMap.toList intmap
