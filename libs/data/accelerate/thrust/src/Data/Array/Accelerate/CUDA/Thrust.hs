{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Array.Accelerate.CUDA.Thrust where

import           Control.Applicative                ((<$>))
import qualified Data.Array.Accelerate              as A
import qualified Data.Array.Accelerate.Array.Sugar  as Sugar (EltRepr, shape, size)
import           Data.Array.Accelerate.Array.Data   (HTYPE_INT)
import           Data.Array.Accelerate.Type         (FloatingType(..), IntegralType(..), NumType(..), numType)
import           Data.Array.Accelerate.CUDA.Foreign (CIO, CUDAForeignAcc(..), DevicePtrs, devicePtrsOfArray, liftIO)
import qualified Foreign.CUDA.Ptr                   as CUDA
import           Foreign.C.Types                    (CInt(..))
import           Foreign.Ptr                        (Ptr)



sort :: forall sh e. (A.Elt e, A.IsNum e, A.Shape sh) => A.Acc (A.Array sh e) -> A.Acc (A.Array sh e)
sort input = A.foreignAcc cudaSort undef input
  where
    cudaSort = CUDAForeignAcc "thrust::sort" sort''
    undef = error "sort undefined for non-CUDA backends"

    sort'' :: A.Array sh e -> CIO (A.Array sh e)
    sort'' arr = do
      let len = Sugar.size $ Sugar.shape arr
      iptr <- devicePointer arr
      liftIO $ execute iptr len

      return arr

    execute :: CUDA.DevicePtr e -> Int -> IO ()
    execute devPtr len = case (numType :: NumType e) of
      FloatingNumType float -> case float of
        TypeFloat{}   -> thrustSortF devPtr len
        TypeDouble{}  -> thrustSortD devPtr len
        TypeCFloat{}  -> thrustSortF (CUDA.castDevPtr devPtr) len
        TypeCDouble{} -> thrustSortD (CUDA.castDevPtr devPtr) len

      IntegralNumType int -> case int of
        TypeInt{}   -> thrustSortI (CUDA.castDevPtr devPtr) len
        TypeWord8{} -> thrustSortW8 devPtr len
        _           -> error $ "Data.Array.Accelerate.CUDA.Thrust: unsupported sorting of given type: " ++ show int

    devicePointer :: A.Array sh e -> CIO (CUDA.DevicePtr e)
    devicePointer v = case (numType :: NumType e) of
      FloatingNumType float -> case float of
        TypeFloat{}   -> singleDevicePointer v
        TypeDouble{}  -> singleDevicePointer v
        TypeCFloat{}  -> CUDA.castDevPtr <$> singleDevicePointer v
        TypeCDouble{} -> CUDA.castDevPtr <$> singleDevicePointer v
      
      IntegralNumType int -> case int of
        TypeInt{}   -> CUDA.castDevPtr <$> singleDevicePointer v
        TypeWord8{} -> singleDevicePointer v
        _           -> error $ "Data.Array.Accelerate.CUDA.Thrust: unsupported sorting of given type: " ++ show int

    singleDevicePointer :: DevicePtrs (Sugar.EltRepr e) ~ ((), CUDA.DevicePtr b) => A.Array sh e -> CIO (CUDA.DevicePtr b)
    singleDevicePointer v = snd <$> devicePtrsOfArray v



foreign import ccall "wrap.h thrustSortF" c_thrustSortF :: Ptr Float -> CInt -> IO ()

thrustSortF :: CUDA.DevicePtr Float -> Int -> IO ()
thrustSortF devPtr size = c_thrustSortF (CUDA.useDevicePtr devPtr) (fromIntegral size)

foreign import ccall "wrap.h thrustSortD" c_thrustSortD :: Ptr Double -> CInt -> IO ()

thrustSortD :: CUDA.DevicePtr Double -> Int -> IO ()
thrustSortD devPtr size = c_thrustSortD (CUDA.useDevicePtr devPtr) (fromIntegral size)

foreign import ccall "wrap.h thrustSortI" c_thrustSortI :: Ptr HTYPE_INT -> CInt -> IO ()

thrustSortI :: CUDA.DevicePtr HTYPE_INT -> Int -> IO ()
thrustSortI devPtr size = c_thrustSortI (CUDA.useDevicePtr devPtr) (fromIntegral size)

foreign import ccall "wrap.h thrustSortW8" c_thrustSortW8 :: Ptr A.Word8 -> CInt -> IO ()

thrustSortW8 :: CUDA.DevicePtr A.Word8 -> Int -> IO ()
thrustSortW8 devPtr size = c_thrustSortW8 (CUDA.useDevicePtr devPtr) (fromIntegral size)
