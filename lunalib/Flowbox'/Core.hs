module Flowbox'.Core (
    module Flowbox'.TH.Inst,
    module Flowbox'.Core,
    module Flowbox'.Common,
    module Data.Tuple.OneTuple
    --module Flowbox'.Data.Tuple.Select
)
where

import Flowbox'.TH.Inst
import Flowbox'.Common
import Data.Tuple.OneTuple
--import Flowbox'.Data.Tuple.Select

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
-- f .: g = \x y->f (g x y)
-- f .: g = (f .) . g
-- (.:) f = ((f .) .)
-- (.:) = (.) (.) (.)
(.:) = (.) . (.)