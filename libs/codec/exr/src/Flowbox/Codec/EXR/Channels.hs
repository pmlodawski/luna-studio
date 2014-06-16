---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TypeOperators #-}

module Flowbox.Codec.EXR.Channels (
      readScanlineChannelR
    , readScanlineChannelA
    , readTiledScanlineChannelR
    , readTileFromChannel'
    , saveTest
    , saveChannels
    ) where

import           Control.Applicative
import           Control.Monad                   (forM, forM_)
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

import Debug.Trace



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
readScanlineChannelA :: EXRFile -> PartNumber -> String -> IO (A.Array (A.Z A.:. Int A.:. Int) CFloat)
readScanlineChannelA exrFile part chanName = do
    (ptr, height, width) <- readScanlineChannel' exrFile part chanName
    let shape = A.Z A.:. height A.:. width
    array <- withForeignPtr ptr $ \p -> A.fromPtr shape ((), castPtr p)
    return array

readTiledScanlineChannel' :: EXRFile -> PartNumber -> String -> IO (ForeignPtr CFloat, Int, Int)
readTiledScanlineChannel' (EXRFile exr) part chanName = do
    (buf, h, w) <- withForeignPtr exr $ \ptr ->
        withCString chanName $ \str ->
        alloca $ \height ->
        alloca $ \width -> do
            putStrLn "before"
            buf <- Bindings.readTiledScanlineChannel ptr (fromIntegral part) str height width
            putStrLn "after"
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
readTileFromChannel' (EXRFile exr) part chanName (xPos, yPos) = do
    (buf, h, w) <- withForeignPtr exr $ \ptr ->
        withCString chanName $ \str ->
        alloca $ \height ->
        alloca $ \width -> do
            let xPos' = fromIntegral xPos
                yPos' = fromIntegral yPos
            --putStrLn "before"
            buf <- Bindings.readTileFromChannel ptr 0 str 0 0 height width
            --putStrLn "after"
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

saveTest :: String -> String -> A.Array A.DIM2 Float -> IO ()
saveTest fileName chanName arr = withCString fileName $ \f ->
    withCString chanName $ \c ->
    mallocBytes size >>= \ptr ->
    A.toPtr arr ((), castPtr ptr) >>
    Bindings.saveTest f c h' w' ptr >>
    free ptr

    where A.Z A.:. h A.:. w = A.arrayShape arr
          w' = fromIntegral w
          h' = fromIntegral h
          size = h * w * sizeOf (undefined :: Float)

foreign import ccall "haskexr.h saveChannelsToFile" saveChannelsC ::
    CString -> Ptr CString -> Ptr (Ptr Float) -> CInt -> CInt -> CInt -> IO ()

saveChannels :: String -> [(String, A.Array A.DIM2 Float)] -> IO ()
saveChannels fileName channels = do
    let A.Z A.:. height A.:. width = A.arrayShape $ snd $ head channels
        (names, chans) = unzip channels
        len = fromIntegral $ length channels
    withCStringList names $ \cstrs ->
        withCString fileName $ \file ->
        withAccArrays chans $ \bufs ->
            saveChannelsC file cstrs bufs len (fromIntegral height) (fromIntegral width)

-- FIXME[mm]: really unsafe, use Mutable Vector instead of modifying Storable Vector in place
withAccArrays :: [A.Array A.DIM2 Float] -> (Ptr (Ptr Float) -> IO a) -> IO a
withAccArrays arrays f = do
    let len = length arrays
    allocaArray len $ \ptr -> do
        foreigns <- forM (zip [0..] arrays) $ \(i, a) -> do
            let ((), vector) = A.toVectors a
                (buf, _, _) = SV.unsafeToForeignPtr vector
                bufPtr = unsafeForeignPtrToPtr buf
            pokeElemOff ptr i bufPtr
            return buf
        ret <- f ptr
        forM_ foreigns $ \fptr -> do
            touchForeignPtr fptr
        return ret

withCStringList :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringList strs f = do
    let len = length strs
    allocaArray len $ \ptr -> do
        forM_ (zip [0..] strs) $ \(i, s) -> do
            cstr <- newCString s
            pokeElemOff ptr i cstr
        ret <- f ptr
        forM_ [0..len-1] $ \i -> do
            cstr <- peekElemOff ptr i
            free cstr
        return ret