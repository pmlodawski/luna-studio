---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
module Flowbox.Codec.EXR.Internal.Types where

import Foreign.ForeignPtr



-- |The representation tag for pointers to objects of IlmImf::MultiPartInputFile C++ class
data EXR

-- |Opaque type representing OpenEXR file. It automatically closes file and deallocates
--  C++-side object on garbage collection.
newtype EXRFile = EXRFile (ForeignPtr EXR)

instance Show EXRFile where
    show _ = "EXRFile"


-- |In multi-part OpenEXR files, each part has a number from 0 to n. In general, passing an incorrect part number
--  will cause an exception on C++ side that's going to crash whole program.
type PartNumber = Int


-- |Type representing all available types of a part in an OpenEXR file.
data PartType
	-- |Flat, scan-line based image
	= ScanlineImage
	-- |Flat, tiled image
    | TiledImage
    -- |Deep, scan-line based image
    | DeepScanline
    -- |Deep, tiled image
    | DeepTile
    -- |Unknown image, OpenEXR files with this type are invalid
    | Unknown String
    deriving Show



data Point a = Point { x :: a
                     , y :: a
                     }

instance Show a => Show (Point a) where
    show (Point x' y') = "(" ++ show x' ++ "," ++ show y' ++ ")"



data Box = Box { min :: Point Int
               , max :: Point Int
               }

instance Show Box where
    show (Box x' y') = "(" ++ show x' ++ "," ++ show y' ++ ")"
