module Test.Hspec.LunaTypechecker.FileSystem where


import Flowbox.Prelude



strictReadFile :: String -> IO String
strictReadFile fileName = do
    tmp <- readFile fileName
    tmp `seq` return tmp