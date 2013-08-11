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
        , "print"
        ]

fnames = map (++"'") names

indent    = replicate 4 ' '
nl        = "\n"
mpostfix  = "''M"
header    = "{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}\n"
modprefix = "Flowbox.Luna.FClasses"
cprefix   = "C''"

genC name = header ++ nl ++ head ++ cls where
    cname     = cprefix ++ name
    head      = "module " ++ modprefix ++ "." ++ cname ++ " where\n\n"
    cls       = clsheader ++ clsbody
    clsheader = "class "  ++ cname ++ " a b | a -> b where\n"
    clsbody   =  indent ++ name ++          "    :: a -> b\n"
              ++ indent ++ name ++ mpostfix ++ " :: a -> IO b\n"

main = do
    let
        defs = map genC fnames
        filesnames = map (++".hs") $ map (cprefix++) fnames

    zipWithM writeFile filesnames defs
    return ()



