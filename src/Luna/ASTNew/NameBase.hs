module Luna.ASTNew.NameBase (
    NameBase(..)
  ) where


import           Flowbox.Prelude
import           Luna.ASTNew.Label      (Label(Label))
import qualified Luna.ASTNew.Name.Path as NamePath
import           Luna.ASTNew.Name.Path (NamePath)


class NameBase a where
  -- | Retrieve the human readable name. In case of variables, it's their usual name as opposed to internal schemes: NamePath or ID.
  nameBase :: a -> String


instance NameBase NamePath where
  nameBase name = name ^. NamePath.base

instance (Show l, Show a) => NameBase (Label l a) where
  nameBase (Label l _) = show l
