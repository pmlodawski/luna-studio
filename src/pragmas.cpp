#define typedArg(a,t) let {_ = a `ofType` (anyType :: m t);};
#define typed(a,t)    ((a) `ofType` (anyType :: m t))
#define name(n)       (Proxy :: Proxy n)
#define member(n)     member name(n)

-- #define rtup0           ()
-- #define rtup1(t1)       (t1,())
-- #define rtup2(t1,t2)    (t1,(t2,()))
-- #define rtup3(t1,t2,t3) (t1,(t2,(t3,())))