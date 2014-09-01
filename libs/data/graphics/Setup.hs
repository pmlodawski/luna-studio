---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

import qualified Data.List                          as List
import           Distribution.Simple
import           Distribution.PackageDescription
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PreProcess
import qualified System.FilePath                    as FilePath

main = defaultMainWithHooks simpleUserHooks { hookedPreProcessors = [("rgb", rgbProfileGen)] }

rgbProfileGen :: BuildInfo -> LocalBuildInfo -> PreProcessor
rgbProfileGen _ _ = PreProcessor {
      platformIndependent = True
    , runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity -> do
        putStrLn $ "Processing " ++ inFile
        source <- readFile inFile
        let moduleName = concat $ List.intersperse "." $ tail $ FilePath.splitDirectories $ FilePath.dropExtension inFile
            [name, whitepoint, forwardMatrix, inverseMatrix] = lines source
            [m11, m12, m13, m21, m22, m23, m31, m32, m33] = words forwardMatrix
            [invM11, invM12, invM13, invM21, invM22, invM23, invM31, invM32, invM33] = words inverseMatrix
            contents = unlines
                [ "{-# LANGUAGE FlexibleInstances     #-}"
                , "{-# LANGUAGE MultiParamTypeClasses #-}"
                , "{-# LANGUAGE TypeFamilies          #-}"
                , ""
                , "module " ++ moduleName ++ " where"
                , ""
                , "import qualified Linear"
                , "import           Linear.Accelerate ()"
                , ""
                , "import Flowbox.Graphics.Color.CIE.XYZ"
                , "import Flowbox.Graphics.Color.Illuminants"
                , "import Flowbox.Graphics.Color.Profile"
                , "import Flowbox.Graphics.Color.RGB"
                , "import Flowbox.Prelude"
                , ""
                , ""
                , ""
                , "newtype " ++ name ++ " a = " ++ name ++ " (RGB a)"
                , "                        deriving Show"
                , ""
                , "instance (Num a, Fractional a) => RGBProfile " ++ name ++ " a where"
                , "    type ReferenceWhite " ++ name ++ " = " ++ whitepoint
                , ""
                , "    toXYZ   (" ++ name ++ " (RGB r g b)) = XYZ x y z"
                , "        where Linear.V3 x y z = m Linear.!* rgb"
                , "              m = Linear.V3 (Linear.V3 " ++ parens m11 ++ " " ++ parens m12 ++ " " ++ parens m13 ++ ")"
                , "                            (Linear.V3 " ++ parens m21 ++ " " ++ parens m22 ++ " " ++ parens m23 ++ ")"
                , "                            (Linear.V3 " ++ parens m31 ++ " " ++ parens m32 ++ " " ++ parens m33 ++ ")"
                , "              rgb = Linear.V3 r g b"
                , ""
                , "    fromXYZ (XYZ x y z) = " ++ name ++ " $ RGB r g b"
                , "        where Linear.V3 r g b = m Linear.!* xyz"
                , "              m = Linear.V3 (Linear.V3 " ++ parens invM11 ++ " " ++ parens invM22 ++ " " ++ parens invM33 ++ ")"
                , "                            (Linear.V3 " ++ parens invM11 ++ " " ++ parens invM22 ++ " " ++ parens invM33 ++ ")"
                , "                            (Linear.V3 " ++ parens invM11 ++ " " ++ parens invM22 ++ " " ++ parens invM33 ++ ")"
                , "              xyz = Linear.V3 x y z"
                , ""]

        writeFile outFile contents
    }

parens s = '(' : s ++ ")"