---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Target.HS.AST.Deriving where

import           Prelude (String, show)
import qualified Prelude as P

data Deriving = Eq
              | Ord
              | Enum
              | Ix
              | Bounded
              | Read
              | Show
              | Generic
              | Typeable
              | GenNewtype String
              deriving (P.Show)


showDeriving :: Deriving -> String
showDeriving = \case
    GenNewtype s -> s
    a            -> show a


stdDerivings :: [Deriving]
stdDerivings = [ Show, Eq, Ord, Generic, Typeable ]