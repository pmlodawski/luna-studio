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
  nameBase :: a -> String


instance NameBase NamePath where
  nameBase name = name ^. NamePath.base

instance (Show l) => NameBase (Label l a) where
  nameBase lab = lab ^. label . to show

instance NameBase (Pat.Pat lab) where
  nameBase (Pat.Var {Pat._vname = vname}) = nameBase vname
  -- Wojtek, zabijesz mnie za to :D Ale nie jestem pewny jak to ma być wyświetlane,
  -- a że w tej sekundzie tylko to do debugu leci to... /KGądek
  nameBase (Pat.App      {}) = "<nameBase for (Pat.Pat lab):App>"
  nameBase (Pat.Typed    {}) = "<nameBase for (Pat.Pat lab):Typed>"
  nameBase (Pat.Grouped  {}) = "<nameBase for (Pat.Pat lab):Grouped>"
  nameBase (Pat.Lit      {}) = "<nameBase for (Pat.Pat lab):Lit>"
  nameBase (Pat.Tuple    {}) = "<nameBase for (Pat.Pat lab):Tuple>"
  nameBase (Pat.Con      {}) = "<nameBase for (Pat.Pat lab):Con>"
  nameBase (Pat.Var      {}) = "<nameBase for (Pat.Pat lab):Var>"
  nameBase (Pat.Wildcard {}) = "<nameBase for (Pat.Pat lab):Wildcard>"
  nameBase (Pat.RecWildcard) = "<nameBase for (Pat.Pat lab):RecWildcard>"


instance NameBase Name.VName  where nameBase (Name.VName  np) = nameBase np
instance NameBase Name.TName  where nameBase (Name.TName  np) = nameBase np
instance NameBase Name.CName  where nameBase (Name.CName  np) = nameBase np
instance NameBase Name.TVName where nameBase (Name.TVName np) = nameBase np