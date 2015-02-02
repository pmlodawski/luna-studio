module Luna.Typechecker.Debug.HumanName (HumanName(humanName)) where


import            Flowbox.Prelude

import qualified  Luna.Syntax.Name          as Name
import            Luna.Syntax.Name.Path     (NamePath, base)
import qualified  Luna.Syntax.Pat           as Pat



class HumanName a where
  humanName :: a -> Text -- ^ Retrieve the name.


instance HumanName (Pat.Pat lab) where
  humanName (Pat.Var {Pat._vname = vname}) = humanName vname
  humanName (Pat.App      {})              = "<HumanName for (Pat.Pat lab):App>"
  humanName (Pat.Typed    {})              = "<HumanName for (Pat.Pat lab):Typed>"
  humanName (Pat.Grouped  {})              = "<HumanName for (Pat.Pat lab):Grouped>"
  humanName (Pat.Lit      {})              = "<HumanName for (Pat.Pat lab):Lit>"
  humanName (Pat.Tuple    {})              = "<HumanName for (Pat.Pat lab):Tuple>"
  humanName (Pat.Con      {})              = "<HumanName for (Pat.Pat lab):Con>"
  humanName (Pat.Wildcard {})              = "<HumanName for (Pat.Pat lab):Wildcard>"
  humanName (Pat.RecWildcard)              = "<HumanName for (Pat.Pat lab):RecWildcard>"


instance (HumanName a) => HumanName (Name.VName  a) where humanName (Name.VName  np) = humanName np
instance (HumanName a) => HumanName (Name.TName  a) where humanName (Name.TName  np) = humanName np
instance (HumanName a) => HumanName (Name.CName  a) where humanName (Name.CName  np) = humanName np
instance (HumanName a) => HumanName (Name.TVName a) where humanName (Name.TVName np) = humanName np

instance HumanName NamePath where
  humanName name = name ^. base