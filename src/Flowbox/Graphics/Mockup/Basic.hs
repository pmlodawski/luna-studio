---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE ViewPatterns    #-}

module Flowbox.Graphics.Mockup.Basic where

import qualified Codec.Picture.Png                 as Juicy
import qualified Codec.Picture.Types               as Juicy
import qualified Data.Array.Accelerate             as A
import qualified Data.Array.Accelerate.Array.Sugar as A
import qualified Data.Array.Accelerate.CUDA        as CUDA (run)
import qualified Data.Array.Accelerate.IO          as A
import           Data.Char                         (toLower)
import           Data.Maybe                        (fromJust)
import qualified Data.Vector.Storable              as SV
import qualified System.FilePath                   as FilePath


import qualified Flowbox.Graphics.Color.Color          as Color
import           Flowbox.Graphics.Image.Channel        (Channel (..), ChannelData (..))
import qualified Flowbox.Graphics.Image.Channel        as Channel
import           Flowbox.Graphics.Image.Error          as Image
import           Flowbox.Graphics.Image.Image          (Image)
import qualified Flowbox.Graphics.Image.Image          as Image
import           Flowbox.Graphics.Image.IO.ImageMagick (loadImage)
import           Flowbox.Graphics.Image.IO.OpenEXR     (readFromEXR)
import           Flowbox.Graphics.Image.View           (View (..))
import qualified Flowbox.Graphics.Image.View           as View
import qualified Flowbox.Graphics.Utils.Utils          as U
import           Flowbox.Math.Matrix                   as M
import           Flowbox.Prelude                       as P hiding (lookup, view)

import Control.PolyApplicative ((<<*>>))
import Luna.Target.HS          (Pure (..), Safe (..), Value (..), autoLift, autoLift1, fromValue, val)



-- == MOST BASIC

-- FIXME[MD,KM]: something should be done with this
temporaryBackend :: M.Backend
temporaryBackend = CUDA.run

pattern VPS x = Value (Pure (Safe x))
type VPS x = Value Pure Safe x

unpackLunaVar :: VPS a -> a
unpackLunaVar (Value (Pure (Safe a))) = a

unpackLunaList :: [VPS a] -> [a]
unpackLunaList = fmap unpackLunaVar

unpackAccDims :: (A.Exp Int,A.Exp Int) -> (Int,Int)
unpackAccDims (x,y) =
  let
    pair = A.lift (x,y) :: A.Exp (Int,Int)
    scalarPair = A.unit pair :: A.Acc (A.Scalar (Int,Int))
    l = temporaryBackend $ scalarPair :: A.Scalar (Int,Int)
    sh' = A.toList l :: [(Int,Int)]
    x' = fst $ head sh'
    y' = snd $ head sh'
  in
    (x',y')


-- == LOAD / SAVE

testLoadRGBA' :: Value Pure Safe String -> Value IO Safe (Value Pure Safe (Matrix2 Float), Value Pure Safe (Matrix2 Float), Value Pure Safe (Matrix2 Float), Value Pure Safe (Matrix2 Float))
testLoadRGBA' path = autoLift1 ((fmap.fmap) (over each val) $ testLoadRGBA) path

testLoadRGBA :: FilePath -> IO (Matrix2 Float, Matrix2 Float, Matrix2 Float, Matrix2 Float)
testLoadRGBA filename = do
    file <- loadImage filename
    case file of
        Right mat -> return $ M.unzip4 $ M.map (convert . A.unpackRGBA32) (Raw mat)
        Left e -> error $ "Unable to load file: " P.++ show e
    where convert t = let (r, g, b, a) = A.unlift t :: (A.Exp A.Word8, A.Exp A.Word8, A.Exp A.Word8, A.Exp A.Word8)
                      in A.lift (A.fromIntegral r / 255, A.fromIntegral g / 255, A.fromIntegral b / 255, A.fromIntegral a / 255)

testSaveRGBA :: FilePath -> Matrix2 Float -> Matrix2 Float -> Matrix2 Float -> Matrix2 Float -> IO ()
testSaveRGBA filename r g b a = saveImageJuicy filename $ compute' temporaryBackend $ M.map A.packRGBA32 $ M.zip4 (conv r) (conv g) (conv b) (conv a)
    where conv = M.map (A.truncate . (* 255.0) . U.clamp' 0 1)

saveImageJuicy :: (SV.Storable a, Elt e, A.Vectors (A.EltRepr e) ~ ((), SV.Vector a))
               => FilePath -> A.Array ((Z :. Int) :. Int) e -> IO ()
saveImageJuicy file matrix = do
    let ((), vec) = A.toVectors matrix
        A.Z A.:. h A.:. w = A.arrayShape matrix
    Juicy.writePng file $ (Juicy.Image w h (SV.unsafeCast vec) :: Juicy.Image Juicy.PixelRGBA8)

loadImageLuna :: FilePath -> IO Image
loadImageLuna path = do
    (r, g, b, a) <- testLoadRGBA path
    let view = insertChannelFloats (View.emptyDefault) [
                   ("rgba.r", r)
                 , ("rgba.g", g)
                 , ("rgba.b", b)
                 , ("rgba.a", a)
               ]
        image = Image.singleton view
    return image

saveImageLuna :: FilePath -> Image -> IO Image
saveImageLuna path img = do
    let (r, g, b, a) = unsafeGetChannels img
    testSaveRGBA path r g b a
    return img

readFromEXRLuna :: FilePath -> IO Image
readFromEXRLuna path = fmap fromJust $ readFromEXR path

extension :: FilePath -> (FilePath, String)
extension path = (path, P.map toLower $ FilePath.takeExtension path)

pattern ImageEXR path <- (extension -> (path, ".exr"))

realReadLuna :: FilePath -> IO Image
realReadLuna (ImageEXR path) = readFromEXRLuna path
realReadLuna path            = loadImageLuna path


-- == HELPERS

onEach :: (A.Exp Float -> A.Exp Float) -> Image -> Image
onEach f = Image.map (View.map $ Channel.unsafeMap (Channel.FunFloat f))

-- onEachRGBA :: forall a. (A.Elt a, A.IsFloating a)
--            => (A.Exp a -> A.Exp a)
--            -> (A.Exp a -> A.Exp a)
--            -> (A.Exp a -> A.Exp a)
--            -> (A.Exp a -> A.Exp a)
--            -> Image
--            -> Image
-- onEachRGBA fr fg fb fa img = Image.appendMultiToPrimary [r,g,b,a] img
--     where r = updateChan fr "rgba.r"
--           g = updateChan fg "rgba.g"
--           b = updateChan fb "rgba.b"
--           a = updateChan fa "rgba.a"
--           updateChan f = case (A.floatingType :: A.FloatingType a) of
--                              A.TypeFloat{}  -> Channel.unsafeMap (Channel.FunFloat f) . getChan
--                              A.TypeDouble{} -> Channel.unsafeMap (Channel.FunDouble f) . getChan
--                              _              -> error "onEachRGBA: invalid type of floating point value"
--           getChan chanName = let Right (Just chan) = Image.getFromPrimary chanName img in chan

onEachRGBA :: (A.Exp Float -> A.Exp Float)
           -> (A.Exp Float -> A.Exp Float)
           -> (A.Exp Float -> A.Exp Float)
           -> (A.Exp Float -> A.Exp Float)
           -> Image
           -> Image
onEachRGBA fr fg fb fa img = Image.appendMultiToPrimary [r,g,b,a] img
    where r = updateChan fr "rgba.r"
          g = updateChan fg "rgba.g"
          b = updateChan fb "rgba.b"
          a = updateChan fa "rgba.a"
          updateChan f = Channel.unsafeMap (Channel.FunFloat f) . getChan
          getChan chanName = let Right (Just chan) = Image.getFromPrimary chanName img in chan

onEachColorRGB :: (A.Exp (Color.RGB Float) -> A.Exp (Color.RGB Float)) -> Image -> Image
onEachColorRGB f img = img'
    where rgb = unsafeGetRGB img
          Right view = Image.lookupPrimary img
          rgb' = M.map f rgb
          unzipRGB = M.unzip3 . M.map (\(A.unlift -> Color.RGB x y z) -> A.lift (x, y, z))

          (r', g', b') = unzipRGB rgb'

          view' = insertChannelFloats view [
                      ("rgba.r", r')
                    , ("rgba.g", g')
                    , ("rgba.b", b')
                  ]

          img' = Image.insertPrimary view' img

onEachRGBAChannels :: (Channel -> Channel)
                   -> (Channel -> Channel)
                   -> (Channel -> Channel)
                   -> (Channel -> Channel)
                   -> Image
                   -> Image
onEachRGBAChannels fr fg fb fa img = img'
  where ChannelFloat _ (MatrixData r) = fr (getChan "rgba.r")
        ChannelFloat _ (MatrixData g) = fg (getChan "rgba.g")
        ChannelFloat _ (MatrixData b) = fb (getChan "rgba.b")
        ChannelFloat _ (MatrixData a) = fa (getChan "rgba.a")

        Right view = Image.lookupPrimary img

        view' = insertChannelFloats view [
                    ("rgba.r", r)
                  , ("rgba.g", g)
                  , ("rgba.b", b)
                  , ("rgba.a", a)
                ]

        img' = Image.insertPrimary view' img
        getChan chanName = let Right (Just chan) = Image.getFromPrimary chanName img in chan


onEachChannel :: (Channel -> Channel) -> Image -> Image
onEachChannel f = Image.map $ View.map f

unsafeGetRGB :: Image -> M.Matrix2 (Color.RGB Float)
unsafeGetRGB img = rgb
    where (r, g, b, _) = unsafeGetChannels img

          rgb = M.zipWith3 (\x y z -> A.lift $ Color.RGB x y z) r g b

unsafeGetChannels :: Image -> (M.Matrix2 Float, M.Matrix2 Float, M.Matrix2 Float, M.Matrix2 Float)
unsafeGetChannels img = (r, g, b, a)
    where Right view = Image.lookupPrimary img
          Right (Just (ChannelFloat _ (Channel.asMatrixData -> MatrixData r))) = View.get view "rgba.r"
          Right (Just (ChannelFloat _ (Channel.asMatrixData -> MatrixData g))) = View.get view "rgba.g"
          Right (Just (ChannelFloat _ (Channel.asMatrixData -> MatrixData b))) = View.get view "rgba.b"
          Right (Just (ChannelFloat _ (Channel.asMatrixData -> MatrixData a))) = View.get view "rgba.a"

insertChannelFloats :: View -> [(String, Matrix2 Float)] -> View
insertChannelFloats view chans = foldr f view chans
    where f (name, chan) acc = View.append (ChannelFloat name . MatrixData $ chan) acc

onImageRGBA :: (A.Exp Float -> A.Exp Float)
            -> (A.Exp Float -> A.Exp Float)
            -> (A.Exp Float -> A.Exp Float)
            -> (A.Exp Float -> A.Exp Float)
            -> Image
            -> Image
onImageRGBA fr fg fb fa img = img'
    where (r, g, b, a) = unsafeGetChannels img
          r' = M.map fr r
          g' = M.map fg g
          b' = M.map fb b
          a' = M.map fa a

          Right view = Image.lookupPrimary img
          view' = insertChannelFloats view [
                      ("rgba.r", r')
                    , ("rgba.g", g')
                    , ("rgba.b", b')
                    , ("rgba.a", a')
                    ]
          img' = Image.insertPrimary view' img

channelToImageRGBA :: Matrix2 Float -> Image
channelToImageRGBA m = image
    where image = Image.singleton view
          view = insertChannelFloats (View.emptyDefault) [
                     ("rgba.r", m)
                   , ("rgba.g", m)
                   , ("rgba.b", m)
                   , ("rgba.a", alpha)
                 ]

          alpha :: Matrix2 Float
          alpha = M.generate (M.shape m) (const 1)

getChannelLuna :: String -> String -> Image -> Image.Result (Maybe Channel)
getChannelLuna viewName channelName img = case Image.lookup viewName img of
    Right view -> View.get view channelName
    _          -> Left $ Image.ViewLookupError viewName

getChannelFromPrimaryLuna :: String -> Image -> Image.Result (Maybe Channel)
getChannelFromPrimaryLuna channelName img = case Image.lookupPrimary img of
    Right view -> View.get view channelName
    _          -> Left $ Image.ViewLookupError "primary view"


-- == OTHER SHIT

liftF6 f t1 t2 t3 t4 t5 t6 = do
    t1' <- t1
    t2' <- t2
    t3' <- t3
    t4' <- t4
    t5' <- t5
    t6' <- t6
    val f <<*>> t1' <<*>> t2' <<*>> t3' <<*>> t4' <<*>> t5' <<*>> t6'

liftF13 fun a b c d e f g h i j k l m = do
    a' <- a
    b' <- b
    c' <- c
    d' <- d
    e' <- e
    f' <- f
    g' <- g
    h' <- h
    i' <- i
    j' <- j
    k' <- k
    l' <- l
    m' <- m
    val fun <<*>> a' <<*>> b' <<*>> c' <<*>> d' <<*>> e' <<*>> f'
            <<*>> g' <<*>> h' <<*>> i' <<*>> j' <<*>> k' <<*>> l' <<*>> m'
