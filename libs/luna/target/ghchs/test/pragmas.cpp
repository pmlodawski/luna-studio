#define _typedArg(a,t) let {_ = a `ofType` (anyType :: m t);};
#define typed(a,t)    ((a) `ofType` (anyType :: m t))
#define _name(n)       (Proxy :: Proxy n)
#define _member(n)     member _name(n)
#define _nuSigArg(n)   nuSigArg (Proxy::Proxy n)
#define _npSigArg(n,a) npSigArg (Proxy::Proxy n) (a)


#define _rtup0           ()
#define _rtup1(t1)       (t1,())
#define _rtup2(t1,t2)    (t1,(t2,()))
#define _rtup3(t1,t2,t3) (t1,(t2,(t3,())))