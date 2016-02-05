---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}

module Flowbox.Graphics.Mockup.Basic (
    InterpolationFilter(..),
    VPS,
    pattern VPS,
    applyTime,
    channelToImageRGBA,
    constantBoundaryWrapper,
    getChannelFromPrimaryLuna,
    getChannelLuna,
    insertChannelFloats,
    loadImageLuna,
    multisampleChannelsLuna,
    onEach,
    onEachChannel,
    onEachColorRGB,
    onEachColorRGBA,
    onEachRGBA,
    onEachRGBAChannels,
    onShader,
    printfInt,
    realReadLuna,
    saveImageLuna,
    temporaryBackend,
    unpackAccDims,
    unpackLunaList,
    unpackLunaVar,
    unsafeGetChannels,
    unsafeGetRGB,
    withAlpha,
) where

import qualified Codec.Picture.Png                     as Juicy
import qualified Codec.Picture.Types                   as Juicy
import qualified Data.Array.Accelerate                 as A
import qualified Data.Array.Accelerate.Array.Sugar     as A
import qualified Data.Array.Accelerate.CUDA            as CUDA (run)
import qualified Data.Array.Accelerate.IO              as A
import           Data.Char                             (toLower)
import           Data.Maybe                            (fromJust)
import qualified Data.Vector.Storable                  as SV
import           Math.Coordinate.Cartesian             (Point2 (..))
import           Math.Space.Space                      (Grid (..))
import qualified System.FilePath                       as FilePath
import           Text.Printf

import           Flowbox.Data.FilePath                 (applyTime)
import qualified Flowbox.Graphics.Color.Color          as Color
import qualified Flowbox.Graphics.Composition.Filter   as Filter
import           Flowbox.Graphics.Image.Channel        (Channel (..), ChannelData (..))
import qualified Flowbox.Graphics.Image.Channel        as Channel
import           Flowbox.Graphics.Image.Error          as Image
import           Flowbox.Graphics.Image.Image          (Image)
import qualified Flowbox.Graphics.Image.Image          as Image
import           Flowbox.Graphics.Image.IO.ImageMagick (loadImage)
import           Flowbox.Graphics.Image.IO.OpenEXR     (readFromEXR)
import           Flowbox.Graphics.Image.View           (View (..))
import qualified Flowbox.Graphics.Image.View           as View
import qualified Flowbox.Graphics.Shader.Matrix        as Shader
import qualified Flowbox.Graphics.Shader.Rasterizer    as Shader
import           Flowbox.Graphics.Shader.Sampler       (Sampler)
import qualified Flowbox.Graphics.Shader.Sampler       as Sampler
import           Flowbox.Graphics.Shader.Shader        (CartesianShader, DiscreteShader, Shader (..))
import           Flowbox.Graphics.Utils.Accelerate     (variable)
import qualified Flowbox.Graphics.Utils.Utils          as U
import           Flowbox.Math.Matrix                   as M
import           Flowbox.Prelude                       as P hiding (lookup, view)

import           Control.PolyApplicative               ((<<*>>))
import           Data.TupleList                        (curryTuple8, curryTuple9)
import           Luna.Target.HS                        (Pure (..), Safe (..), Value (..), autoLift, autoLift1, fromValue, val)



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

unsafeFromMaybe :: Maybe a -> a
unsafeFromMaybe (Just a) = a

printfInt :: String -> Int -> String
printfInt str int = printf str int

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

onEachRGBA :: (A.Exp Float -> A.Exp Float)
           -> (A.Exp Float -> A.Exp Float)
           -> (A.Exp Float -> A.Exp Float)
           -> (A.Exp Float -> A.Exp Float)
           -> Image
           -> Image
onEachRGBA fr fg fb fa img = Image.appendMultiToPrimary [r, g, b, a] img
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

onEachColorRGBA :: (A.Exp (Color.RGBA Float) -> A.Exp (Color.RGBA Float)) -> Image -> Image
onEachColorRGBA f img = Image.appendMultiToPrimary (P.zipWith makeChan channels [r',g',b',a']) img
    where makeChan name mat  = ChannelFloat name $ MatrixData mat
          (r', g', b', a')   = unzipRGBA rgba'
          unzipRGBA          = M.unzip4 . M.map (\(A.unlift -> Color.RGBA a b c d) -> A.lift (a, b, c, d))
          rgba'              = M.map f rgba
          rgba               = M.zipWith4 (\a b c d -> A.lift $ Color.RGBA a b c d) r g b a
          Right [r, g, b, a] = (fmap.fmap) (unsafeGetMat . Channel.asMatrix . unsafeFromMaybe)
                             $ Image.getChannelsFromPrimary channels img
          unsafeGetMat (ChannelFloat _ (MatrixData mat)) = mat
          channels           = fmap ((P.++) "rgba.") ["r", "g", "b", "a"]

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

unsafeGetRGBA :: Image -> M.Matrix2 (Color.RGBA Float)
unsafeGetRGBA img = M.zipWith4 (\v x y z -> A.lift $ Color.RGBA v x y z) r g b a
    where Right [r,g,b,a] = (fmap.fmap) (unsafeGetMat . Channel.asMatrix . unsafeFromMaybe)
                          $ Image.getChannelsFromPrimary channels img
          unsafeGetMat (ChannelFloat _ (MatrixData mat)) = mat
          channels        = fmap ((P.++) "rgba.") ["r", "g", "b", "a"]

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

onShader f img = img'
    where (r, g, b, a) = unsafeGetChannels img & over each (Shader.rasterizer . f . Shader.fromMatrix (A.Constant 0))
          Right view = Image.lookupPrimary img
          view' = insertChannelFloats view [
                      ("rgba.r", r)
                    , ("rgba.g", g)
                    , ("rgba.b", b)
                    , ("rgba.a", a)
                  ]
          img' = Image.insertPrimary view' img

withAlpha :: (A.Exp Float -> A.Exp Float -> A.Exp Float) -> Image -> Image
withAlpha f img = img'
    where (r, g, b, a) = unsafeGetChannels img
          r' = M.zipWith f r a
          g' = M.zipWith f g a
          b' = M.zipWith f b a

          Right view = Image.lookupPrimary img
          view' = insertChannelFloats view [
                      ("rgba.r", r')
                    , ("rgba.g", g')
                    , ("rgba.b", b')
                    , ("rgba.a", a)
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

fromPolarMapping :: (Elt a, IsFloating a, Elt e) => CartesianShader (Exp a) (Exp e) -> CartesianShader (Exp a) (Exp e)
fromPolarMapping (Shader cnv gen) = Shader cnv $ \(Point2 x y) ->
    let Grid cw ch = fmap A.fromIntegral cnv
        radius = (sqrt $ x * x + y * y) / (sqrt $ cw * cw + ch * ch)
        angle  = atan2 y x / (2 * pi)
    in gen (Point2 (angle * cw) (radius * ch))

toPolarMapping :: (Elt a, IsFloating a, Elt e) => CartesianShader (Exp a) (Exp e) -> CartesianShader (Exp a) (Exp e)
toPolarMapping (Shader cnv gen) = Shader cnv $ \(Point2 angle' radius') ->
    let Grid cw ch = fmap A.fromIntegral cnv
        angle = (angle' / cw) * 2 * pi
        radius = (radius' / ch) * (sqrt $ cw * cw + ch * ch)
    in gen (Point2 (radius * cos angle) (radius * sin angle))

constantBoundaryWrapper :: a -> MValue a
constantBoundaryWrapper v = MValue (return v) (const $ return ())

data InterpolationFilter a = NearestNeighbour
                           | Box
                           | Basic
                           | Triangle
                           | Bell
                           | BSpline
                           | Lanczos a
                           | Polynomial a a
                           | Mitchell
                           | CatmullRom
                           | Gauss a
                           | Dirac a
                           deriving (Show, Functor)

toInterpolator :: (Elt e, IsFloating e) => InterpolationFilter (Exp e) -> DiscreteShader (Exp e) -> CartesianShader (Exp e) (Exp e)
toInterpolator = \case
    NearestNeighbour -> Sampler.nearest
    Box              -> Sampler.interpolator Filter.box
    Basic            -> Sampler.interpolator Filter.basic
    Triangle         -> Sampler.interpolator Filter.triangle
    Bell             -> Sampler.interpolator Filter.bell
    BSpline          -> Sampler.interpolator Filter.bspline
    Lanczos a        -> Sampler.interpolator $ Filter.lanczos a
    Polynomial a b   -> Sampler.interpolator $ Filter.polynomial a b
    Mitchell         -> Sampler.interpolator Filter.mitchell
    CatmullRom       -> Sampler.interpolator Filter.catmulRom
    Gauss a          -> Sampler.interpolator $ Filter.gauss a
    Dirac a          -> Sampler.interpolator $ Filter.dirac a

-- TODO[KM]: ask someone what is this function supposed to do? now it might be obsolete to pattern match on MatrixData and perform M.map, doing this through Shaders would probably be a better idea
-- TODO^:    commented out for now(the boundary can't be explicitly typed here, we can have different data – why even try to interpolate all channels at the same time?)
--interpolateChannelsLuna :: A.Boundary Double -> InterpolationFilter Double -> Image -> Image
--interpolateChannelsLuna (fmap variable -> boundary) (toInterpolator . fmap variable -> interpol) = Image.map (View.map interpolate)
--    where interpolate (ChannelFloat name (asMatrixData -> MatrixData mat)) = ChannelFloat name $ ContinuousData $ toGen $ mat
--          interpolate (ChannelInt   name (asMatrixData -> MatrixData mat)) = ChannelInt   name $ ContinuousData $ toGen . M.map A.fromIntegral $ mat
--          interpolate (ChannelBit   name (asMatrixData -> MatrixData mat)) = ChannelBit   name $ ContinuousData $ toGen . M.map (A.fromIntegral . A.boolToInt) $ mat

--          toGen = interpol . fromMatrix boundary

toMultisampler :: Grid (Exp Int) -> InterpolationFilter (Exp Float) -> Sampler Float
toMultisampler grid = \case
    NearestNeighbour -> Sampler.monosampler
    Box              -> Sampler.multisampler $ Filter.normalize $ Filter.toMatrix grid Filter.box
    Basic            -> Sampler.multisampler $ Filter.normalize $ Filter.toMatrix grid Filter.basic
    Triangle         -> Sampler.multisampler $ Filter.normalize $ Filter.toMatrix grid Filter.triangle
    Bell             -> Sampler.multisampler $ Filter.normalize $ Filter.toMatrix grid Filter.bell
    BSpline          -> Sampler.multisampler $ Filter.normalize $ Filter.toMatrix grid Filter.bspline
    Lanczos a        -> Sampler.multisampler $ Filter.normalize $ Filter.toMatrix grid $ Filter.lanczos a
    Polynomial a b   -> Sampler.multisampler $ Filter.normalize $ Filter.toMatrix grid $ Filter.polynomial a b
    Mitchell         -> Sampler.multisampler $ Filter.normalize $ Filter.toMatrix grid Filter.mitchell
    CatmullRom       -> Sampler.multisampler $ Filter.normalize $ Filter.toMatrix grid Filter.catmulRom
    Gauss a          -> Sampler.multisampler $ Filter.normalize $ Filter.toMatrix grid $ Filter.gauss a
    Dirac a          -> Sampler.multisampler $ Filter.normalize $ Filter.toMatrix grid $ Filter.dirac a

multisampleChannelsLuna :: Grid Int -> InterpolationFilter Float -> Image -> Image
multisampleChannelsLuna (fmap variable -> grid) (toMultisampler grid . fmap variable -> sampler :: Sampler Float) = Image.map (View.map multisample)
    where multisample (Channel.asContinuous -> ChannelFloat name (ContinuousData gen)) = ChannelFloat name $ MatrixData . Shader.rasterizer . sampler $ gen
          --                                                            FIXME[MM]: ^ we don't want this here,
          --                                                                         but ChannelShader requires ContinuousShader :/
          --                                                            FIXME[KM]: ^ I've changed the structure of Channels so we might have to talk about this
          multisample channel                     = channel

-- FIXME[MM]: will remove the whole view if removing fails - it should somehow propagate the error
-- FIXME[KM][iup]: when fixing, we also have take into consideration the change to the Image.update function
--removeChannelLuna :: String -> String -> Image -> Image
--removeChannelLuna viewName channelName = Image.update f viewName
--    where f view = case View.remove channelName view of
--                  Left _ -> Nothing
--                  Right v -> Just v

-- FIXME[KM]: [iup]
--insertChannelLuna :: String -> Channel -> Image -> Image
--insertChannelLuna viewName chan = Image.update f viewName
--    where f = Just . View.append chan

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


