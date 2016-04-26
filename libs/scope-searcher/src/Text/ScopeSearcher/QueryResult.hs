module Text.ScopeSearcher.QueryResult where

import           Control.Lens
import           Data.Text.Lazy  (Text)

data Highlight = Highlight { start :: Int
                           , len   :: Int
                           } deriving (Show, Eq)

data QueryResult = QueryResult { _prefix     :: Text
                               , _name       :: Text
                               , _fullname   :: Text
                               , _highlights :: [Highlight]
                               , _tpe        :: Text
                               } deriving (Eq, Show)

-- makeLenses ''Highlight
makeLenses ''QueryResult
