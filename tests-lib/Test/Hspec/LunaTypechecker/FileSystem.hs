module Test.Hspec.LunaTypechecker.FileSystem where


strictReadFile :: String -> IO String
strictReadFile fileName = do
    tmp <- readFile fileName
    tmp `seq` return tmp