import Control.Monad (zipWithM)

names = map (("select" ++) . show) [0..10] ++
        [ "add"
        , "x'getter"
        , "y'getter"
        , "z'getter"
        , "x'setter"
        , "y'setter"
        , "z'setter"
        , "incx"
        , "init"
        , "print"
        ]

--fnames = map (++"'") names

indent    = replicate 4 ' '
nl        = "\n"
mpostfix  = "''M"
header    = "{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}\n"
modprefix = "Flowbox.Luna.FClasses"
cprefix   = "C''"
fprefix   = "U'"

genC name = header ++ nl ++ head ++ cls where
    fname     = name ++ "'"
    cname     = fprefix ++ name
    cfname    = cprefix ++ fname
    head      = "module " ++ modprefix ++ "." ++ cname ++ " where\n\n"
    cls       = clsheader ++ clsbody
    clsheader = "class "  ++ cfname ++ " a b | a -> b where\n"
    clsbody   =  indent ++ fname ++          "    :: a -> b\n"
              ++ indent ++ fname ++ mpostfix ++ " :: a -> IO b\n"

main = do
    let
        defs = map genC names
        filesnames = map (++".hs") $ map (fprefix++) names

    zipWithM writeFile filesnames defs
    return ()



