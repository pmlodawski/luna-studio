---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Luna.AST.Lit.Number where

import           Control.Lens
import           Flowbox.Generics.Deriving.QShow
import           Flowbox.Prelude                 (Eq, Maybe, Read, Show)
import qualified Flowbox.Prelude                 as P
import           GHC.Generics



data Sign = Positive
          | Negative
          deriving (Show, Eq, Generic, Read)


data Repr = Float   { _int :: P.String, _frac :: P.String }
          | Decimal { _int :: P.String }
          deriving (Show, Eq, Generic, Read)


type Base = P.Int

data Number = Number { _base :: Base
                     , _repr :: Repr
                     , _exp  :: Maybe Number
                     , _sign :: Sign
                     }
            deriving (Show, Eq, Generic, Read)


instance QShow Sign
instance QShow Repr
instance QShow Number
makeLenses ''Repr
makeLenses ''Number


binary, ternary, quaternary, quinary, senary, septenary, octal, nonary, decimal, undecimal, duoDecimal, tridecimal, tetradecimal, pentadecimal, hexadecimal, septendecimal, octodecimal, nonadecimal, vigesimal, unovigesimal, duovigesimal, triovigesimal, quadrovigesimal, pentavigesimal, hexavigesimal, heptovigesimal, ocotovigesimal, novovigesimal, trigesimal, unotrigesimal, duotrigesimal, triotrigesimal, quadrotrigesimal, pentatrigesimal,
 hexatrigesimal :: Repr -> Maybe Number -> Sign -> Number
binary           = Number 2
ternary          = Number 3
quaternary       = Number 4
quinary          = Number 5
senary           = Number 6
septenary        = Number 7
octal            = Number 8
nonary           = Number 9
decimal          = Number 10
undecimal        = Number 11
duoDecimal       = Number 12
tridecimal       = Number 13
tetradecimal     = Number 14
pentadecimal     = Number 15
hexadecimal      = Number 16
septendecimal    = Number 17
octodecimal      = Number 18
nonadecimal      = Number 19
vigesimal        = Number 20
unovigesimal     = Number 21
duovigesimal     = Number 22
triovigesimal    = Number 23
quadrovigesimal  = Number 24
pentavigesimal   = Number 25
hexavigesimal    = Number 26
heptovigesimal   = Number 27
ocotovigesimal   = Number 28
novovigesimal    = Number 29
trigesimal       = Number 30
unotrigesimal    = Number 31
duotrigesimal    = Number 32
triotrigesimal   = Number 33
quadrotrigesimal = Number 34
pentatrigesimal  = Number 35
hexatrigesimal   = Number 36


