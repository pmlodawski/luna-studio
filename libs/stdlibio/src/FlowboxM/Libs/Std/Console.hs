{-# LANGUAGE TemplateHaskell,
             FlexibleInstances,
             MultiParamTypeClasses,
             UndecidableInstances #-}

module FlowboxM.Libs.Std.Console where

--import           Flowbox.Luna.Helpers.Core
--import           Prelude hiding (print)
--import qualified Prelude as Prelude
--import           Flowbox.Luna.FClasses.U_print

--data Console = Console deriving (Show)

--cons_Console = Pure Console

--newtype CField_print  a = CField_print  { getCField_print  :: a } deriving (Show)


--print_T = CField_print 
--mkInst ''FC_print 'print_T 'print

--_print self v = Prelude.print v

--get1_print_T x v1    = _print (getCField_print x) v1 
--mkInst ''Get1 'get1_print_T 'get1

--get0_print_T x       = _print (getCField_print x) (Pure (""::String))
--mkInst ''Get0 'get0_print_T 'get0