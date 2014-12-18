module Luna.ASTNew.NameBase (
    NameBase(..)
  ) where


import            Flowbox.Prelude
import            Luna.ASTNew.Label     (Label, label)
import qualified  Luna.ASTNew.Name      as Name
import qualified  Luna.ASTNew.Name.Path as NamePath
import            Luna.ASTNew.Name.Path (NamePath)
import qualified  Luna.ASTNew.Pat       as Pat


class NameBase a where
  -- | Retrieve the human readable name. In case of variables, it's their usual name as opposed to internal schemes: NamePath or ID.
  nameBase :: a -> Text


instance NameBase NamePath where
  nameBase name = name ^. NamePath.base

instance (Show l, Show a) => NameBase (Label l a) where
  nameBase lab = lab ^. label . to show . to fromString
