{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}


module Main where


import            Luna.Data.Namespace                       (Namespace (Namespace))
import            Luna.Data.Source                          (Medium (Text), Source (Source))

import qualified  Luna.Pass                                 as Pass
import qualified  Luna.Pass2.Analysis.Struct                as P2SA
--import qualified  Luna.Pass2.Target.HS.HASTGen              as P2HASTGen
--import qualified  Luna.Pass2.Target.HS.HSC                  as P2HSC
import qualified  Luna.Pass2.Transform.Desugar.ImplicitSelf as P2ImplSelf
import qualified  Luna.Pass2.Transform.Hash                 as P2Hash
import qualified  Luna.Pass2.Transform.Parse.Stage1         as P2Stage1
import qualified  Luna.Pass2.Transform.Parse.Stage2         as P2Stage2
import qualified  Luna.Pass2.Transform.SSA                  as P2SSA

import            Control.Monad.IO.Class                    (liftIO)
import            Control.Monad.Trans.Either
import            Data.List                                 (intercalate)
import            Data.Text.Lazy                            (pack)
import            Text.Show.Pretty                          (ppShow)


import            Inference                                 as FooInfer


main :: IO ()
main = do f_print [Bold,Green] "MAIN"
          f_print [Cyan] "…reading `src/Maintest.luna`"
          maintest_luna <- do tmp <- readFile "src/Maintest.luna"
                              tmp `seq` return tmp
          f_print [Cyan] "…passes"
          let src = Source "Maintest_luna" (Text $ pack maintest_luna)

          result <- runEitherT $ do
            (ast1, astinfo1) <- Pass.run1_ P2Stage1.pass src
            sa1              <- Pass.run1_ P2SA.pass ast1
            (ast2, astinfo2) <- Pass.run3_ P2Stage2.pass (Namespace [] sa1) astinfo1 ast1
            (ast3, astinfo3) <- Pass.run2_ P2ImplSelf.pass astinfo2 ast2
            sa2              <- Pass.run1_ P2SA.pass ast3
            constraints      <- Pass.run2_ FooInfer.tcpass ast3 sa2
            ast4             <- Pass.run1_ P2Hash.pass ast3
            ast5             <- Pass.run1_ P2SSA.pass ast4
            --hast             <- Pass.run1_ P2HASTGen.pass ast5
            --hsc              <- Pass.run1_ P2HSC.pass hast
            liftIO $ writeAST " 1.1. Transform.Parse.Stage1         : ast1"        $ ppShow ast1
            liftIO $ writeAST " 1.2. Transform.Parse.Stage1         : astinfo1"    $ ppShow astinfo1
            liftIO $ writeAST " 2.   Analysis.Struct                : sa1"         $ ppShow sa1
            liftIO $ writeAST " 3.1. Transform.Parse.Stage2         : ast2"        $ ppShow ast2
            liftIO $ writeAST " 3.2. Transform.Parse.Stage2         : astinfo2"    $ ppShow astinfo2
            liftIO $ writeAST " 4.1. Transform.Desugar.ImplicitSelf : ast3"        $ ppShow ast3
            liftIO $ writeAST " 4.2. Transform.Desugar.ImplicitSelf : astinfo3"    $ ppShow astinfo3
            liftIO $ writeAST " 5.   Pass2.Analysis.Struct          : sa2"         $ ppShow sa2
            liftIO $ writeAST " 6.   Typechecker                    : constraints" $ ppShow constraints
            liftIO $ writeAST " 7.   Transform.Hash                 : ast4"        $ ppShow ast4
            liftIO $ writeAST " 8.   Transform.SSA                  : ast5"        $ ppShow ast5
            -- liftIO $ writeAST " 9.   Target.HS.HASTGen              : hast"        $ ppShow $ hast
            -- liftIO $ writeAST "10.   Target.HS.HSC                  : hsc"         $ unpack $ hsc
            return  ()

          case result of
            Left _   -> f_print [Red, Bold] "some error, sorry"
            Right () -> return ()


writeAST :: FilePath -> String -> IO ()
writeAST path str = do
  let filepath = "tmp/" ++ path
  writeFile filepath str
  f_print [Cyan] $ "…writing " ++ show filepath

printer :: (Show a) => String -> a -> IO ()
printer x y = printer_aux x (show y)

printer_aux :: String -> String -> IO ()
printer_aux x y = do  f_print [Bold,White] "\n-----------------------------------------------------------------------------"
                      putStr "> "
                      f_print [Yellow] x
                      f_print [Bold,White] "-----------------------------------------------------------------------------\n"
                      putStrLn y

section :: IO () -> IO ()
section sec = do  sec
                  f_print [Bold, White] "\n\n#############################################################################\n\n"

f_print :: [PrintAttrs] -> String -> IO ()
f_print fs x = do let fmt = intercalate ";" (fmap (show.attrtonum) fs)
                  putStr $ "\x1b[" ++ fmt ++ "m"
                  putStr x
                  putStrLn "\x1b[0m"

data PrintAttrs = Black
                | Red
                | Green
                | Yellow
                | Blue
                | Magenta
                | Cyan
                | White
                | Bold

attrtonum :: PrintAttrs -> Int
attrtonum Black   = 30
attrtonum Red     = 31
attrtonum Green   = 32
attrtonum Yellow  = 33
attrtonum Blue    = 34
attrtonum Magenta = 35
attrtonum Cyan    = 36
attrtonum White   = 37
attrtonum Bold    = 1
