module Main where


import Test.Hspec.LunaTypechecker
import Spec



main :: IO ()
main = hspec $ parallel spec