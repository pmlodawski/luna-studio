module Text.Doc.Utils where

import           Data.Monoid
import           Text.Parsec         hiding (many, optional, parse, (<|>))

(++) :: Monoid a => a -> a -> a
(++) = mappend

surround :: Stream s m t => ParsecT s u m close -> ParsecT s u m a -> ParsecT s u m a
surround a = between a a
