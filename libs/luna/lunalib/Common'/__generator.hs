import           Control.Monad   (zipWithM)

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

indent    = replicate 4 ' '
nl        = "\n"
mpostfix  = "''M"
header    = "{-# LANGUAGE FunctionalDependencies, FlexibleInstances #-}\n"
modprefix = "Common'"
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
        defs = map genC names
        fnames = map (++".hs") $ map (cprefix++) names

    zipWithM writeFile fnames defs
    return ()



