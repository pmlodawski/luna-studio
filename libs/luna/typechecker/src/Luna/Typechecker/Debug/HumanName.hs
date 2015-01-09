module Luna.Typechecker.Debug.HumanName (HumanName(humanName)) where

import qualified  Luna.ASTNew.Name          as Name
import            Luna.ASTNew.Name.Path     (NamePath, base)
import qualified  Luna.ASTNew.Pat           as Pat

import            Control.Lens
import            Data.Text.Lazy            (Text, pack)

class HumanName a where
  -- | Retrieve the name.
  humanName :: a -> Text


instance HumanName (Pat.Pat lab) where
  humanName (Pat.Var {Pat._vname = vname}) = humanName vname
  humanName (Pat.App      {})              = pack "<HumanName for (Pat.Pat lab):App>"
  humanName (Pat.Typed    {})              = pack "<HumanName for (Pat.Pat lab):Typed>"
  humanName (Pat.Grouped  {})              = pack "<HumanName for (Pat.Pat lab):Grouped>"
  humanName (Pat.Lit      {})              = pack "<HumanName for (Pat.Pat lab):Lit>"
  humanName (Pat.Tuple    {})              = pack "<HumanName for (Pat.Pat lab):Tuple>"
  humanName (Pat.Con      {})              = pack "<HumanName for (Pat.Pat lab):Con>"
  humanName (Pat.Wildcard {})              = pack "<HumanName for (Pat.Pat lab):Wildcard>"
  humanName (Pat.RecWildcard)              = pack "<HumanName for (Pat.Pat lab):RecWildcard>"


instance (HumanName a) => HumanName (Name.VName  a) where humanName (Name.VName  np) = humanName np
instance (HumanName a) => HumanName (Name.TName  a) where humanName (Name.TName  np) = humanName np
instance (HumanName a) => HumanName (Name.CName  a) where humanName (Name.CName  np) = humanName np
instance (HumanName a) => HumanName (Name.TVName a) where humanName (Name.TVName np) = humanName np

instance HumanName NamePath where
  humanName name = name ^. base