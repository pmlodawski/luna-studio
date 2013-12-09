{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

module FlowboxM.Luna.Show (
        module FlowboxM.Luna.Show,
        lrepr,
        LRepr
) where

import Data.Tuple.OneTuple
import Data.Typeable                (Typeable, typeOf)
import FlowboxM.Luna.Data
import FlowboxM.Utils.Generics.Repr
import FlowboxM.Utils.Generics.Show

instance (Typeable a) => Show (IO a) where
    show e = '(' : (show . typeOf) e ++ ")"

instance (Typeable a, Typeable b) => Show (a -> b) where
    show e = '(' : (show . typeOf) e ++ ")"


deriving instance Generic (OneTuple a)
deriving instance Generic ((,,,,,,,) v1 v2 v3 v4 v5 v6 v7 v8)
deriving instance Generic ((,,,,,,,,) v1 v2 v3 v4 v5 v6 v7 v8 v9)
deriving instance Generic ((,,,,,,,,,) v1 v2 v3 v4 v5 v6 v7 v8 v9 v10)

instance LRepr () where
    lrepr _ = "{}"

instance (LRepr v1) => LRepr (OneTuple v1) where
    lrepr (OneTuple v1) = "{" ++ lrepr v1 ++ "}"

instance (LRepr v1, LRepr v2) => LRepr (v1,v2)
instance (LRepr v1, LRepr v2, LRepr v3) => LRepr (v1,v2,v3)
instance (LRepr v1, LRepr v2, LRepr v3, LRepr v4) => LRepr (v1,v2,v3,v4)
instance (LRepr v1, LRepr v2, LRepr v3, LRepr v4, LRepr v5) => LRepr (v1,v2,v3,v4,v5)
instance (LRepr v1, LRepr v2, LRepr v3, LRepr v4, LRepr v5, LRepr v6) => LRepr (v1,v2,v3,v4,v5,v6)
instance (LRepr v1, LRepr v2, LRepr v3, LRepr v4, LRepr v5, LRepr v6, LRepr v7) => LRepr (v1,v2,v3,v4,v5,v6,v7)
instance (LRepr v1, LRepr v2, LRepr v3, LRepr v4, LRepr v5, LRepr v6, LRepr v7, LRepr v8) => LRepr (v1,v2,v3,v4,v5,v6,v7,v8)
instance (LRepr v1, LRepr v2, LRepr v3, LRepr v4, LRepr v5, LRepr v6, LRepr v7, LRepr v8, LRepr v9) => LRepr (v1,v2,v3,v4,v5,v6,v7,v8,v9)
instance (LRepr v1, LRepr v2, LRepr v3, LRepr v4, LRepr v5, LRepr v6, LRepr v7, LRepr v8, LRepr v9, LRepr v10) => LRepr (v1,v2,v3,v4,v5,v6,v7,v8,v9,v10)


instance LRepr a => LRepr (Pure a)

instance Show a => Show (Pure a) where
    show (Pure a) = "Pure " ++ show a
