#!/bin/bash
function realpath()
{
    f=$@
    if [ -d "$f" ]; then
        base=""
        dir="$f"
    else
        base="/$(basename "$f")"
        dir=$(dirname "$f")
    fi
    dir=$(cd "$dir" && /bin/pwd)
    echo "$dir$base"
}

runhaskell                                                                                         \
    -XDoAndIfThenElse                                                                              \
    -XNoImplicitPrelude                                                                            \
    -XTemplateHaskell                                                                              \
    -XOverloadedStrings                                                                            \
    -XDeriveGeneric                                                                                \
    -XStandaloneDeriving                                                                           \
    -XFlexibleContexts                                                                             \
    -XMultiParamTypeClasses                                                                        \
    -XDefaultSignatures                                                                            \
    -XDeriveFunctor                                                                                \
    -XDeriveTraversable                                                                            \
    -XDeriveFoldable                                                                               \
    -XFlexibleInstances                                                                            \
    -XLambdaCase                                                                                   \
    -XConstraintKinds                                                                              \
    -XGeneralizedNewtypeDeriving                                                                   \
    -XDeriveDataTypeable                                                                           \
    -XEmptyDataDecls                                                                               \
    -XTupleSections                                                                                \
    -XTypeOperators                                                                                \
    -XDataKinds                                                                                    \
    -XInstanceSigs                                                                                 \
    -XViewPatterns                                                                                 \
    -XTypeFamilies                                                                                 \
    -i$NBO/libs/utils/src                                                                          \
    -i$NBO/libs/luna/typechecker/src                                                               \
    -i$HS_PUBLIC_LIBS/convert/src                                                                  \
    -i$HS_PUBLIC_LIBS/data-repr/src                                                                \
    -i$HS_PUBLIC_LIBS/data-layer/src                                                               \
    -i$HS_PUBLIC_LIBS/data-rtuple/src                                                              \
    -i$HS_PUBLIC_LIBS/prologue/src                                                                 \
    -i$HS_PUBLIC_LIBS/typelevel/src                                                                \
    -i$HS_PUBLIC_LIBS/container/src                                                                \
    -i$HS_PUBLIC_LIBS/functor-utils/src                                                            \
    -i$HS_PUBLIC_LIBS/lens-utils/src                                                               \
    -i$HS_PUBLIC_LIBS/data-construction/src                                                        \
    -package-db=$HS_SBOX_PGS                                                                       \
    -ddump-splices                                                                                 \
    $NBO/libs/luna/typechecker/test/Main.hs                                              