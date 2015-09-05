{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Data.Containers.Poly where

import Prologue
import           Data.TypeLevel.List (In)
import Data.TypeLevel.Bool
import Data.Typeable

newtype OptBuilder (opts :: [*]) a = OptBuilder a


class FuncBuilder f a | a -> f where
    buildFunc :: f -> a

class FuncTrans opts f a | a opts -> f where
    transFunc :: OptBuilder opts f -> a

------------------

type family CmpLst (lst :: [k]) (lst' :: [k']) :: [Bool] where
            CmpLst (l ': ls)    (l  ': ls')     = (True  ': CmpLst ls ls')
            CmpLst (l ': ls)    (l' ': ls')     = (False ': CmpLst ls ls')
            CmpLst '[]          '[]             = '[]

type family LstIn (lst :: [k]) (lst' :: [k']) :: [Bool] where
            LstIn (l ': ls) lst = In l lst ': LstIn ls (Remove l lst)
            LstIn ls        '[] = '[]
            LstIn '[]       lst = '[]


type family   Remove (a :: k) (cont :: c) :: c
type instance Remove (a :: k) ((t ': ts) :: [k]) = If (a :== t) ts (t ': Remove a ts)
type instance Remove (a :: k) '[]                = '[]


type family ModsOf cont (inst :: k) :: [*]
data Mods (opts :: [*]) = Mods
--data Mods (mods :: [Bool]) = Mods
data InstMods (inst :: k) (mods :: [Bool]) = InstMods
data InstMods2 (inst :: k) (mods :: [Bool]) (cont :: *) = InstMods2
data InstModsX (inst :: k) (query :: [*]) (mods :: [Bool]) (cont :: *) = InstModsX

type I = InstMods2
type I2X = InstModsX

type family UpdateQuery instMods guery where
    UpdateQuery (InstModsX inst q m cont) q'= InstModsX inst q' (LstIn (ModsOf cont inst) q') cont



type family Foxo inst query where
    Foxo (inst query mods cont) query' = inst query' (LstIn (ModsOf cont inst) query') cont

type family Lolo a inst where
    Lolo a ((inst :: [*] -> [Bool] -> * -> k) query mods cont) = (inst (a ': query) (LstIn (ModsOf cont inst) (a ': query)) cont)

type family Lolo2 a inst where
    Lolo2 a ((inst :: [*] -> [Bool] -> * -> k) query mods cont) = (inst (a ': query) (LstIn (ModsOf cont inst) (a ': query)) cont)

    --Lolo ((inst :: [*] -> [Bool] -> * -> k) query mods cont) a = inst query mods cont
    --Lolo ((inst :: [*] -> [Bool] -> * -> k) query mods cont) a = inst (a ': query) (LstIn (ModsOf cont inst) (a ': query)) cont


polyspec :: InstMods2 inst mods cont -> InstMods2 inst mods cont'
polyspec _ = InstMods2

rebaseSpec :: InstMods2 inst mods cont -> InstMods2 inst' mods cont
rebaseSpec _ = InstMods2


rebaseSpecX :: InstModsX inst q mods cont -> InstModsX inst' q (LstIn (ModsOf cont inst') q) cont
rebaseSpecX _ = InstModsX

polySpecX :: InstModsX inst q mods cont -> InstModsX inst q (LstIn (ModsOf cont' inst) q) cont'
polySpecX _ = InstModsX
--type family Rebase mods base where Rebase (InstMods2 inst mods old) new = InstMods2 inst mods new

query :: InstModsX inst q mods cont -> Proxy q
query _ = Proxy

type family Falses (lst :: [k]) :: [Bool] where
    Falses (a ': as) = False ': Falses as
    Falses '[]       = '[]

-- FIXME[wd]: Inst' nie dziala - np. w Resizable.hs
type Inst'     (inst :: [Bool] -> * -> k)      cont     = Inst inst ('[] :: [Bool]) cont
type Inst      (inst :: [Bool] -> * -> k) mods cont     =          inst (LstIn (ModsOf cont inst) mods)    cont
type InstFunc  (inst :: [Bool] -> * -> k) mods cont out = InstMods inst (LstIn (ModsOf cont inst) mods) -> cont -> out
type InstFunc2 (inst :: [Bool] -> * -> k) mods cont sig = InstMods2 inst (LstIn (ModsOf cont inst) mods) cont -> sig

type InstX     (inst :: [*] -> [Bool] -> * -> k) mods cont     = inst mods (LstIn (ModsOf cont inst) mods)    cont
type InstFuncX (inst :: [*] -> [Bool] -> * -> k) mods cont sig = InstModsX inst mods (LstIn (ModsOf cont inst) mods) cont -> sig

type InstX3      (inst :: [Bool] -> * -> k) mods cont     =          inst (LstIn (ModsOf cont inst) mods)    cont


type InstX2    (inst :: [*] -> [Bool] -> * -> k) mods     = inst mods '[]



type SchemeBuilder2 cont mods sig scheme = forall inst. FuncBuilder (Mods mods -> sig) scheme => InstFunc2 inst mods cont sig -> scheme


type DOO mods c a inst cont = (FuncBuilder (Mods mods -> sig) a) => (InstMods2 inst (LstIn (ModsOf cont inst) mods) cont -> sig) -> a

type DOOX query c a inst cont = (FuncBuilder (Mods query -> sig) a) => (InstModsX inst query (LstIn (ModsOf cont inst) query) cont -> sig) -> a

buildScheme2 :: DOO mods c a inst cont
buildScheme2 f = buildFunc (flip runMods2 f)

buildSchemeX :: DOOX query c a inst cont
buildSchemeX f = buildFunc (flip runModsX f)

--modFuncX :: (FuncTrans '[] (Mods query -> sig) b) => (InstModsX inst query (LstIn (ModsOf cont inst) query) cont -> sig) -> b
modFuncX :: InstFuncX inst mods cont sig -> Func mods sig
modFuncX = transFunc . buildSchemeX

modFunc2 :: InstFunc2 inst mods cont sig -> Func mods sig
modFunc2 = transFunc . buildScheme2

--modFunc2' :: _ => _
--modFunc2' = transFunc . buildScheme2


runMods2 :: (LstIn (ModsOf cont inst) mods ~ matches) => Mods mods -> (InstMods2 inst matches cont -> sig) -> sig
runMods2 _ f = f InstMods2

runModsX :: (LstIn (ModsOf cont inst) query ~ matches) => Mods query -> (InstModsX inst query matches cont -> sig) -> sig
runModsX _ f = f InstModsX

type Scheme               sig = FuncBuilder                 sig  f => f
type ModFunc     sel mods sig = FuncTrans sel (Mods mods -> sig) f => f
type Func            mods sig = ModFunc   '[]       mods    sig



type SchemeBuilder inst cont mods out scheme = FuncBuilder (Mods mods -> cont -> out) scheme => InstFunc inst mods cont out -> scheme


buildScheme :: SchemeBuilder inst cont mods out scheme
buildScheme f = undefined

modFunc :: InstFunc inst mods cont sig -> Func mods (cont -> sig)
modFunc = transFunc . buildScheme



emptyOpts = Mods :: Mods '[]



type EmptyPolyLst = ('[] :: [k])

instance {-# OVERLAPPABLE #-} (f ~ a, g ~ b)             => FuncBuilder (f -> g)            (a -> b)            where buildFunc = id
instance {-# OVERLAPPABLE #-} (t ~ (f -> g), opts ~ '[]) => FuncBuilder (f -> g)            (OptBuilder opts t) where buildFunc = OptBuilder

instance                                    (opts ~ opts')       => FuncTrans opts f  (OptBuilder opts' f) where transFunc = id
instance                           (f ~ (Mods opts -> a -> b))   => FuncTrans opts f  (a -> b)             where transFunc (OptBuilder f) = f Mods




type EmptyStarLst = ('[] :: [*])



--type family Foo (lst (a :: k) where
--    Foo



--------------



