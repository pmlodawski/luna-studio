module Generator.Cython.Utils where

import Control.Monad       (when)
import Data.String.Utils   (endswith, split)
import Language.Haskell.TH
import System.Directory    (doesFileExist, getDirectoryContents, removeFile)
import System.FilePath     (joinPath)


moduleName :: Q Exp
moduleName = fmap loc_module location >>= \mdl ->  return (LitE (StringL mdl) )


modulePath :: String -> FilePath
modulePath modName = joinPath $ split "." modName


------------------------------------------------------------------------------
-- File utils
------------------------------------------------------------------------------
endsWithPxd :: (Monad m) => [String] -> m [String]
endsWithPxd = return . filter (endswith ".pxd")


getPxdsForDir :: FilePath -> IO [FilePath]
getPxdsForDir dir = getDirectoryContents dir >>= endsWithPxd


appendFileFromDisk :: FilePath -> FilePath -> IO ()
appendFileFromDisk fileTo fileFrom = readFile fileFrom >>= appendFile fileTo


safeRemoveFile :: FilePath -> IO ()
safeRemoveFile fileName = do
    fileExists <- doesFileExist fileName
    when fileExists $ removeFile fileName
