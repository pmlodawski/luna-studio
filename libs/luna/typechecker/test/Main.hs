{-# LANGUAGE RankNTypes #-}

module Main where


import qualified Luna.Parser.Parser as Parser

import qualified Luna.ASTNew.Name   as Name
import qualified Luna.ASTNew.Label  as Label
import qualified Luna.ASTNew.Enum   as Enum
import qualified Luna.ASTNew.Module as Module
import qualified Luna.ASTNew.Unit   as Unit


import Control.Applicative
import Control.Monad

-- we don't need no education
-- we don't need no edward kmett

type Lens s t a b = Functor f => (a -> f b) -> s -> f t

infixl 8 ^.
(^.) :: s -> Lens s t a b -> a
s^.l = getConst (l Const s)


test_foo :: String 
test_foo = unlines  [ "def foo:"
                    , "  123"
                    , ""
                    , "def bar:"
                    , "  456"
                    ]


main :: IO ()
main = do case Parser.parseString test_foo (Parser.moduleParser [Name.TName "ModFoo"] Parser.defState) of
            Right (ast, _) -> do  print "parsing okay"
                                  case Parser.testme ast Parser.defState of
                                    (ast', _) -> do  print "YO"
                                                     ast_ops ast'
            Left err  -> do print "NOPE"
                            print err


--ast_ops :: Unit.Unit (Label.Label Enum.IDTag (Module.Module Enum.IDTag [Char])) -> IO ()
ast_ops (Unit.Unit ast) = do  putStrLn "AST ----------------------------------------------------------------------------"
                              print ast
                              let ast_label   = ast ^. Label.label
                                  ast_module  = ast ^. Label.element
                              putStrLn "LABEL --------------------------------------------------------------------------"
                              print ast_label
                              putStrLn "ELEMENT ------------------------------------------------------------------------"
                              print ast_module
                              let Module.Module module_path module_name module_body = ast_module
                              putStrLn "PATH ---------------------------------------------------------------------------"
                              print module_path
                              putStrLn "NAME ---------------------------------------------------------------------------"
                              print module_name
                              putStrLn "BODY ---------------------------------------------------------------------------"
                              print module_body
                              forM_ module_body $ \fun -> do
                                putStrLn "FUN ----------------------------------------------------------------------------"
                                print fun
                                putStrLn "FUN ELEMENT --------------------------------------------------------------------"
                                let fun_elem = fun ^. Label.element
                                print fun_elem
