module Text.ScopeSearcher.QueryResult where

import           Control.Lens
import           Data.Text.Lazy           (Text)

import           Text.ScopeSearcher.Score (Score)


data Highlight = Highlight { _start :: Int
                           , _len   :: Int
                           } deriving (Show, Eq)

data QueryResult = QueryResult { _prefix     :: Text
                               , _name       :: Text
                               , _fullname   :: Text
                               , _highlights :: [Highlight]
                               , _tpe        :: Text
                               , _score      :: Score
                               } deriving (Show, Eq)

makeLenses ''Highlight
makeLenses ''QueryResult
