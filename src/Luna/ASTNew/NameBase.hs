module Luna.ASTNew.NameBase (
    NameBase(..)
  ) where


import           Flowbox.Prelude
import           Luna.ASTNew.Label      (Label(Label))
import qualified Luna.ASTNew.Name.Multi as MultiName


class NameBase a where
  -- | Retrieve the human readable name. In case of variables, it's their usual name as opposed to internal schemes: MultiName or ID.
  nameBase :: a -> String


instance NameBase (MultiName.MultiName String) where
  nameBase name = name ^. MultiName.base

instance (Show l, Show a) => NameBase (Label l a) where
  nameBase (Label l _) = show l
