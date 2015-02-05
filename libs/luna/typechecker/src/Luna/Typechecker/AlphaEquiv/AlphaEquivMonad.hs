module Luna.Typechecker.AlphaEquiv.AlphaEquivMonad (
      AlphaEquivMonad(..),
      ttInsert, ttLookup, ffInsert, ffLookup
  ) where


import            Flowbox.Prelude
import            Data.Map.Strict             (Map)
import qualified  Data.Map.Strict             as M

import            Luna.Typechecker.Data.TVar



type TypesTranslate  = Map TVar TVar
type FieldsTranslate = Map String String

type Quadruple a b c = b -> b -> c -> c -> (a, b, b, c, c)


newtype AlphaEquivMonad a = AlphaEquivMonad { runAlphaEq  :: Quadruple (Maybe a) FieldsTranslate TypesTranslate }


instance Monad AlphaEquivMonad where
    fail _ = AlphaEquivMonad aux
      where aux fmab fmba ttab ttba = (Nothing,fmab,fmba,ttab,ttba)

    return x = AlphaEquivMonad aux
      where aux fmab fmba ttab ttba = (Just x,fmab,fmba,ttab,ttba)

    m >>= ak = AlphaEquivMonad aux
      where aux fmab fmba ttab ttba =
                case runAlphaEq m fmab fmba ttab ttba of
                    (Nothing,fmab1,fmba1,ttab1,ttba1) -> (Nothing,fmab1,fmba1,ttab1,ttba1)
                    (Just a, fmab1,fmba1,ttab1,ttba1) -> runAlphaEq (ak a) fmab1 fmba1 ttab1 ttba1


-- Type operations


ttInsert :: TVar -> TVar -> AlphaEquivMonad ()
ttInsert a b = AlphaEquivMonad aux 
  where aux fmab fmba ttab ttba = (Just (), fmab, fmba, M.insert a b ttab, M.insert b a ttba)


ttLookup :: TVar -> TVar -> AlphaEquivMonad Bool
ttLookup a b = AlphaEquivMonad aux
  where aux fmab fmba ttab ttba
          | M.lookup a ttab == Just b  && M.lookup b ttba == Just a = (Just True, fmab,fmba,ttab,ttba)
          | M.notMember a ttab         && M.notMember b ttba        = (Just False,fmab,fmba,ttab,ttba)
          | otherwise                                               = (Nothing,   fmab,fmba,ttab,ttba)


-- Field labels operations


ffInsert :: String -> String -> AlphaEquivMonad ()
ffInsert a b = AlphaEquivMonad aux 
  where aux fmab fmba ttab ttba = (Just (), M.insert a b fmab, M.insert b a fmba, ttab, ttba)


ffLookup :: String -> String -> AlphaEquivMonad Bool
ffLookup a b = AlphaEquivMonad aux
  where aux fmab fmba ttab ttba
          | M.lookup a fmab == Just b && M.lookup b fmba == Just a  = (Just True, fmab,fmba,ttab,ttba)
          | M.notMember a fmab        && M.notMember b fmba         = (Just False,fmab,fmba,ttab,ttba)
          | otherwise                                               = (Nothing,   fmab,fmba,ttab,ttba)
