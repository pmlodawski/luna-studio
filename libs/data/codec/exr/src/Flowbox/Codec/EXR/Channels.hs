---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}

module Flowbox.Codec.EXR.Channels (
      readScanlineChannelR
    , readScanlineChannelA
    , readTiledScanlineChannelR
    , readTileFromChannel'
    , readTileFromChannelR
    ) where

import           Control.Applicative
import qualified Data.Array.Accelerate           as A   
import qualified Data.Array.Accelerate.IO        as A
import qualified Data.Array.Repa                 as R
import qualified Data.Array.Repa.Repr.ForeignPtr as R
import qualified Data.Vector.Storable            as SV
import           Foreign
import           Foreign.C.String
import           Foreign.C.Types

import qualified Flowbox.Codec.EXR.Internal.Bindings as Bindings
import           Flowbox.Codec.EXR.Internal.Types



readScanlineChannel' :: EXRFile -> PartNumber -> String -> IO (ForeignPtr CFloat, Int, Int)
readScanlineChannel' (EXRFile exr) part chanName = do
    (buf, h, w) <- withForeignPtr exr $ \ptr ->
        withCString chanName $ \str ->
        alloca $ \height ->
        alloca $ \width -> do
            buf <- Bindings.readScanlineChannel ptr (fromIntegral part) str height width
            h <- fromIntegral <$> peek height
            w <- fromIntegral <$> peek width
            return (buf, h, w)
    fptr <- newForeignPtr finalizerFree buf
    return (fptr, h, w)


-- |Reads a given channel into a Repa array.
readScanlineChannelR :: EXRFile -> PartNumber -> String -> IO (R.Array R.F (R.Z R.:. Int R.:. Int) CFloat)
readScanlineChannelR exrFile part chanName = do
    (ptr, height, width) <- readScanlineChannel' exrFile part chanName
    let shape = R.Z R.:. height R.:. width
    return $ R.fromForeignPtr shape ptr


-- |Reads a given channel into an Accelerate array.
readScanlineChannelA :: EXRFile -> PartNumber -> String -> IO (A.Array (A.Z A.:. Int A.:. Int) Float)
readScanlineChannelA exrFile part chanName = do
    (ptr, height, width) <- readScanlineChannel' exrFile part chanName
    let shape = A.Z A.:. height A.:. width
        storableVector = SV.unsafeFromForeignPtr0 ptr (height * width)
    return $ A.fromVectors shape ((), SV.unsafeCast storableVector)

readTiledScanlineChannel' :: EXRFile -> PartNumber -> String -> IO (ForeignPtr CFloat, Int, Int)
readTiledScanlineChannel' (EXRFile exr) part chanName = do
    (buf, h, w) <- withForeignPtr exr $ \ptr ->
        withCString chanName $ \str ->
        alloca $ \height ->
        alloca $ \width -> do
            buf <- Bindings.readTiledScanlineChannel ptr (fromIntegral part) str height width
            h <- fromIntegral <$> peek height
            w <- fromIntegral <$> peek width
            return (buf, h, w)
    fptr <- newForeignPtr finalizerFree buf
    return (fptr, h, w)


readTiledScanlineChannelR :: EXRFile -> PartNumber -> String -> IO (R.Array R.F (R.Z R.:. Int R.:. Int) CFloat)
readTiledScanlineChannelR exrFile part chanName = do
    (ptr, height, width) <- readTiledScanlineChannel' exrFile part chanName
    let shape = R.Z R.:. height R.:. width
    return $ R.fromForeignPtr shape ptr

type TileCoordinates = (Int, Int)

readTileFromChannel' :: EXRFile -> PartNumber -> String -> TileCoordinates -> IO (ForeignPtr CFloat, Int, Int)
readTileFromChannel' (EXRFile exr) (fromIntegral -> part) chanName (fromIntegral -> xPos, fromIntegral -> yPos) = do
    (buf, h, w) <- withForeignPtr exr $ \ptr ->
        withCString chanName $ \str ->
        alloca $ \height ->
        alloca $ \width -> do
            buf <- Bindings.readTileFromChannel ptr part str xPos yPos height width
            h <- fromIntegral <$> peek height
            w <- fromIntegral <$> peek width
            return (buf, h, w)
    fptr <- newForeignPtr finalizerFree buf
    return (fptr, h, w)

readTileFromChannelR :: EXRFile -> PartNumber -> String -> TileCoordinates -> IO (R.Array R.F (R.Z R.:. Int R.:. Int) CFloat)
readTileFromChannelR exrFile part chanName coords = do
    (ptr, height, width) <- readTileFromChannel' exrFile part chanName coords
    let shape = R.Z R.:. height R.:. width
    return $ R.fromForeignPtr shape ptr


-- not supported, its representation is troublesome to implement
--readDeepScanlineChannel' :: EXRFile -> Int -> String -> IO (ForeignPtr CFloat, Int, Int)
--readDeepScanlineChannel' (EXRFile exr) part chanName = do
--    (buf, h, w) <- withForeignPtr exr $ \ptr ->
--        withCString chanName $ \str ->
--        alloca $ \height ->
--        alloca $ \width -> do
--            buf <- readDeepScanlineChannelC ptr (fromIntegral part) str height width
--            h <- fromIntegral <$> peek height
--            w <- fromIntegral <$> peek width
--            return (buf, h, w)
--    fptr <- newForeignPtr finalizerFree buf
--    return (fptr, h, w)

--readDeepScanlineChannel :: EXRFile -> Int -> String -> IO (Repa.Array Repa.F (Repa.Z Repa.:. Int Repa.:. Int) CFloat)
--readDeepScanlineChannel exrFile part chanName = do
--    (ptr, height, width) <- readDeepScanlineChannel' exrFile part chanName
--    let shape = Repa.Z Repa.:. height Repa.:. width
--    return $ Repa.fromForeignPtr shape ptr
