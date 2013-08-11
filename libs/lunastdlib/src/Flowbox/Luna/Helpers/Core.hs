module Flowbox.Luna.Helpers.Core (
	module Flowbox.Luna.Helpers.Core,
    module Flowbox.Luna.Helpers.TH.Inst,
    module Flowbox.Luna.Libs.Std.Data.NTuple.Select,
    module Flowbox.Luna.Libs.Std.Base
)
where

import Flowbox.Luna.Helpers.TH.Inst
import Flowbox.Luna.Libs.Std.Data.NTuple.Select
import Flowbox.Luna.Libs.Std.Base

(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
-- f .: g = \x y->f (g x y)
-- f .: g = (f .) . g
-- (.:) f = ((f .) .)
-- (.:) = (.) (.) (.)
(.:) = (.) . (.)