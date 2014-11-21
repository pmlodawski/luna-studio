module Main where


import qualified Luna.Parser.Parser as Parser

import qualified Luna.ASTNew.Name   as Name
import qualified Luna.ASTNew.Label  as Label
import qualified Luna.ASTNew.Enum   as Enum
import qualified Luna.ASTNew.Module as Module
import qualified Luna.ASTNew.Unit   as Unit


test_foo :: String 
test_foo = unlines [
            "def foo:", 
            "  123"
           ]


main :: IO ()
main = do case Parser.parseString test_foo (Parser.moduleParser [Name.TName "ModFoo"] Parser.defState) of
            Right (ast, _) -> do  print "parsing okay"
                                  ast_ops ast
            Left err  -> do print "NOPE"
                            print err


ast_ops :: Unit.Unit (Label.Label Enum.IDTag (Module.Module Enum.IDTag [Char])) -> IO ()
ast_ops ast = print ast