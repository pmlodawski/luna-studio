{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Prelude   as P hiding (log, lookup)

import Control.Monad.IO.Class (liftIO)

import System.Log.Level
import System.Log.Log
import System.Log.Data
import System.Log.Format
import System.Log.Logger.Base
import System.Log.Logger.Handler
import System.Log.Logger.Priority
import System.Log.Logger.Thread
import System.Log.Simple

import System.Log.Tuples



test = do
    --let h = flushHandler `addChildHandler` printHandler
    addHandler printHandler
    debug "running subrutine"

    --x <- runWriterLoggerT $ runDupLogger test2

    --liftIO $ print x

    addHandler printHandler


    debug "debug1"
    setPriority Debug

    debug "debug2"
    info "info"
    warning "warning"



    --flush
    liftIO $ print "---"
    fail "oh no"
    --liftIO $ threadDelay 1000000
    liftIO $ print "---"


    critical "ola"

    --mkLog (0::Int) "hello"

    return 11




--class LogConstructor d m where
    --mkLog :: RecordBuilder d -> m ()

--instance Num Level


test2 = do
    debug "debug2"

    return 1

main = do
    --print =<< (runBaseLoggerT (Lvl, Msg) $ runHandlerLoggerT $ runPriorityLoggerT Warning test)
    --print =<< runThreadedLogger (runBaseLoggerT (Lvl,Msg) . runHandlerLoggerT . runPriorityLoggerT Warning) test

    --print $ runIdentity (runBaseLoggerT (Lvl, Msg) test)
    --print =<< (runBaseLoggerT (Lvl, Msg) . runThreadedLogger $ test)

    print =<< (runBaseLoggerT (Lvl, Msg, Time) . runHandlerLoggerT . runPriorityLoggerT Warning . runThreadedLogger $ test)
    --print =<< (runBaseLoggerT (Lvl, Msg) . runThreadedLogger $ test2)

    

    --print "end"
    --threadDelay 1000

    --print =<< (runPriorityLoggerT 1 test)
    --print =<< (runBaseLoggerT (test :: StdLogger IO () ))

    --print =<< (fmap snd $ runWriterLoggerT (test2 :: StdLogger2 IO () ))
    return ()





--data OneTuple a = OneTuple a deriving Show


--class RTupleConv r t | t -> r, r -> t where
--    t2r :: t -> r
--    r2t :: r -> t

--instance RTupleConv () () where
--    t2r = id
--    r2t = id
--instance RTupleConv (a,()) (OneTuple a) where
--    t2r (OneTuple a) = (a,())
--    r2t (a,()) = OneTuple a
--instance RTupleConv (t1,(t2,())) (t1,t2) where
--    t2r (t1,t2) = (t1,(t2,()))
--    r2t (t1,(t2,())) = (t1,t2)
--instance RTupleConv (t1,(t2,(t3,()))) (t1,t2,t3) where
--    t2r (t1,t2,t3) = (t1,(t2,(t3,())))
--    r2t (t1,(t2,(t3,()))) = (t1,t2,t3)
--instance RTupleConv (t1,(t2,(t3,(t4,())))) (t1,t2,t3,t4) where
--    t2r (t1,t2,t3,t4) = (t1,(t2,(t3,(t4,()))))
--    r2t (t1,(t2,(t3,(t4,())))) = (t1,t2,t3,t4)
--instance RTupleConv (t1,(t2,(t3,(t4,(t5,()))))) (t1,t2,t3,t4,t5) where
--    t2r (t1,t2,t3,t4,t5) = (t1,(t2,(t3,(t4,(t5,())))))
--    r2t (t1,(t2,(t3,(t4,(t5,()))))) = (t1,t2,t3,t4,t5)
--instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,())))))) (t1,t2,t3,t4,t5,t6) where
--    t2r (t1,t2,t3,t4,t5,t6) = (t1,(t2,(t3,(t4,(t5,(t6,()))))))
--    r2t (t1,(t2,(t3,(t4,(t5,(t6,())))))) = (t1,t2,t3,t4,t5,t6)
--instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) (t1,t2,t3,t4,t5,t6,t7) where
--    t2r (t1,t2,t3,t4,t5,t6,t7) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,())))))))
--    r2t (t1,(t2,(t3,(t4,(t5,(t6,(t7,()))))))) = (t1,t2,t3,t4,t5,t6,t7)
--instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) (t1,t2,t3,t4,t5,t6,t7,t8) where
--    t2r (t1,t2,t3,t4,t5,t6,t7,t8) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,()))))))))
--    r2t (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,())))))))) = (t1,t2,t3,t4,t5,t6,t7,t8)
--instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9) where
--    t2r (t1,t2,t3,t4,t5,t6,t7,t8,t9) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,())))))))))
--    r2t (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,()))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9)
--instance RTupleConv (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,())))))))))) (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) where
--    t2r (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) = (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,()))))))))))
--    r2t (t1,(t2,(t3,(t4,(t5,(t6,(t7,(t8,(t9,(t10,())))))))))) = (t1,t2,t3,t4,t5,t6,t7,t8,t9,t10)





-- TODO:
--   filters
--   proper handler formatting