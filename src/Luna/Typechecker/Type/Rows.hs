import Luna.Typechecker.Type.Type

data Tyrow = EmptyRow | Row { fName :: Label, fType :: Type, row :: Tyrow } | RowVar Tyvar

-- | Set of labels which the associated tyvar must lack. Usable for kind
-- row.
type Constraint = S.Set Label

singleConstraint :: Label -> Constraint
singleConstraint = S.singleton

instance Types Tyrow where
    ftv (Row { _, fType, record }) = ftv fType `union` ftv record
    ftv RowEmpty = []
    apply s row = row { fType = (apply s (fType row)), record = (apply s (row row)) }
