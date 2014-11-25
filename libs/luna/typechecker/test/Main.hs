{-# LANGUAGE RankNTypes #-}

module Main where


import qualified Luna.Parser.Parser     as Parser

import qualified Luna.ASTNew.Name       as Name
import qualified Luna.ASTNew.Label      as Label
import qualified Luna.ASTNew.Enum       as Enum
import qualified Luna.ASTNew.Module     as Module
import qualified Luna.ASTNew.Unit       as Unit
import qualified Luna.ASTNew.Decl       as Decl
import qualified Luna.ASTNew.Name.Multi as Multi


-- Wojtkizmy
import qualified Luna.Pass as Pass
import qualified Luna.Pass2.Transform.Parse.Stage2 as Stage2
import qualified Luna.Pass2.Transform.Parse.Stage1 as Stage1
import           Luna.Data.Namespace (Namespace(Namespace))
import Luna.Data.Source (Source(Source), Medium(String), Code(Code))
import qualified Luna.Pass2.Analysis.Alias as AA


import Control.Monad.Trans.Either
import Control.Applicative
import Control.Monad

import Control.Lens

import Data.List

-- we dont need no education
-- we dont need no edward kmett

--type Lens s t a b = Functor f => (a -> f b) -> s -> f t


-- TODO: remove lenses from deps :<
--infixl 8 ^.
--(^.) :: s -> Lens s t a b -> a
--s^.l = getConst (l Const s)


test_foo :: String 
test_foo = unlines  [ "def foo a b:"
                    , "  a + b"
                    , ""
                    , "def bar:"
                    , "  456"
                    ]

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

f_print :: [PrintAttrs] -> String -> IO ()
f_print fs x = do let fmt = intercalate ";" (map (show.attrtonum) fs)
                  putStr $ "\x1b[" ++ fmt ++ "m"
                  putStr x
                  putStrLn "\x1b[0m"

--thing :: IO (Either Pass.PassError (Unit.Unit (Label.Label Enum.IDTag (Module.Module Enum.IDTag Stage2.ResultExpr)), Luna.Data.AliasInfo.AliasInfo))
thing = do  let src = Source "ModTestString" (String test_foo)
            res <- runEitherT $ do
              (ast1, astinfo) <- Pass.run1_ Stage1.pass src
              aa1             <- Pass.run1_ AA.pass ast1
              ast2            <- Pass.run3_ Stage2.pass (Namespace [] aa1) astinfo ast1
              aa2             <- Pass.run1_ AA.pass ast2
              return (ast2,aa2)
            return res

main :: IO ()
main = do x <- thing
          case x of
            Left e               -> return ()
            Right (uast, astinfo) -> do let Unit.Unit ast = uast
                                            ast_label     = ast ^. Label.label
                                            ast_module    = ast ^. Label.element
                                        section $ do
                                          printer "LABEL"  ast_label
                                          printer "MODULE" ast_module

                                        let Module.Module module_path module_name module_body = ast_module
                                        section $ do
                                          printer "MODULE PATH" module_path
                                          printer "MODULE NAME" module_name
                                          printer "MODULE BODY" module_body

                                        section $ do
                                          forM_ module_body $ \body_labeled_elem -> do
                                            let body_elem = body_labeled_elem ^. Label.element
                                                fun_label = body_labeled_elem ^. Label.label
                                            case body_elem of
                                              Decl.Function{} -> do let Just fun_name = Multi.toStr <$> (body_elem ^? Decl.fname)
                                                                    printer ("FUN: " ++ fun_name) body_elem
                                              _               -> printer "don't know what is this" body_elem


  where printer :: (Show a) => String -> a -> IO ()
        printer x y = do  f_print [Bold,White] "\n--------------------------------------------------------------------------------"
                          putStr "> "
                          f_print [Yellow] x
                          f_print [Bold,White] "--------------------------------------------------------------------------------\n"
                          print y
        section :: IO () -> IO ()
        section sec = do  sec
                          f_print [Bold, White] "\n\n################################################################################\n\n"

--main = do case Parser.parseString test_foo (Parser.moduleParser [Name.TName "ModFoo"] Parser.defState) of
--            Right (ast, _) -> do  print "parsing okay"
--                                  case Parser.testme ast Parser.defState of
--                                    (ast', _) -> do  print "YO"
--                                                     ast_ops ast'
--            Left err  -> do print "NOPE"
--                            print err


--ast_ops :: Unit.Unit (Label.Label Enum.IDTag (Module.Module Enum.IDTag [Char])) -> IO ()
--ast_ops (Unit.Unit ast) = do  putStrLn "AST ----------------------------------------------------------------------------"
--                              print ast

--                              putStrLn "LABEL --------------------------------------------------------------------------"
--                              print ast_label
--                              putStrLn "ELEMENT ------------------------------------------------------------------------"
--                              print ast_module
--                              let Module.Module module_path module_name module_body = ast_module
--                              putStrLn "PATH ---------------------------------------------------------------------------"
--                              print module_path
--                              putStrLn "NAME ---------------------------------------------------------------------------"
--                              print module_name
--                              putStrLn "BODY ---------------------------------------------------------------------------"
--                              print module_body
                              --forM_ module_body $ \fun -> do
                              --  putStrLn "FUN ----------------------------------------------------------------------------"
                              --  print fun
                              --  putStrLn "FUN ELEMENT --------------------------------------------------------------------"
                              --  let fun_elem = fun ^. Label.element
                              --  print fun_elem
