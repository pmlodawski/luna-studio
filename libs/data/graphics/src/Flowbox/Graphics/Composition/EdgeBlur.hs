
module Flowbox.Graphics.Composition.EdgeBlur (
    BlurType(..) , edges, maskBlur, eee, mixImages
) where

import           Data.Array.Accelerate               as A hiding (constant, filter, scatter, size, stencil)
import           Flowbox.Graphics.Composition.Filter as F
import           Flowbox.Graphics.Prelude            as P hiding (filter)
import           Flowbox.Graphics.Shader.Pipe
import           Flowbox.Graphics.Shader.Shader
import           Flowbox.Graphics.Utils.Accelerate   (variable)
import           Flowbox.Math.Matrix                 as M
import           Math.Space.Space


eee x = x+1


applyKernel :: (IsNum a, Elt a) => Matrix2 a -> DiscreteShader (Exp a) -> DiscreteShader (Exp a)
applyKernel kernel img = process img where
    kernelN = id M.>-> id $ kernel
    p = pipe (A.Constant 0)
    process x = id `p` F.filter 1 kernelN `p` id $ x

  --convolve mode kernel
  --  where fi = fmap A.fromIntegral
  --        mode point offset = fi point + fi offset

-- bluredImg = blur n sigma testShader


mixImages :: (Applicative f, Floating a) => f a -> f a -> f a -> f a
mixImages edgesMask first second = (+) <$>
                               ( (*) <$> first <*> edgesMask )
                               <*>

--mixImages :: (Applicative f, Num a) => f a -> f a -> f a -> f a
--mixImages edgesMask first second = (+) <$>
--                               ( (*) <$> first <*> edgesMask )
--                               <*>
                               ( (*) <$> second <*> fmap (1 -) edgesMask )

detectEdges :: (Elt a, IsFloating a) => Exp a -> DiscreteShader (Exp a) -> DiscreteShader (Exp a)
detectEdges sens img = (\x y -> P.min 1.0 $ sens*x+sens*y) <$>
                       (fmap abs $ applyKernel (M.transpose sobel {-- :: Matrix2 Double --} ) img)
                       <*>
                       (fmap abs $ applyKernel sobel {-- :: Matrix2 Double --} img)

--edgeBlur :: BlurType -> Exp Int -> Exp Double -> Matrix2 Double -> [Matrix2 Double] -> [Matrix2 Double]
--edgeBlur blurType kernelSize edgeMult matteCh chs = fmap ( rasterizer . blurFunc . (fromMatrix Clamp) ) chs where
--    maskEdges =  edges edgeMult  (fromMatrix Clamp matteCh)
--    blurFunc  = maskBlur blurType kernelSize maskEdges --matteCh

--edges :: Exp Double -> Matrix2 Double -> CartesianShader (Exp Double) (Exp Double)

edges :: (Elt a, IsFloating a) => Exp a -> DiscreteShader (Exp a) -> DiscreteShader (Exp a)
edges edgeMult channel = blurFunc bigEdges where
    blurFunc  = blurChoice GaussBlur 15
    bigEdges  = dilate (Grid 5 5) $ erode (Grid 3 3) thinEdges
    thinEdges = detectEdges edgeMult imgShader
    imgShader = channel

maskBlur :: (Elt a, IsFloating a) => BlurType -> Exp Int -> DiscreteShader (Exp a) -> DiscreteShader (Exp a) -> DiscreteShader (Exp a)
maskBlur blurType kernelSize mask img = mixImages mask blured imgShader where
    imgShader = img
      --maskEdges = mask --blur 15 5.0 $  nearest $  dilate (Grid 10 10) $ {-- erode (Grid 3 3) $ --} monosampler $  detectEdges $  nearest $ fromMatrix Clamp mask
    blurFunc  = blurChoice blurType
    blured    = blurFunc kernelSize imgShader

blurChoice :: (Elt a, IsFloating a) => BlurType -> Exp Int -> DiscreteShader (Exp a) -> DiscreteShader (Exp a)
blurChoice blurType = case blurType of
  GaussBlur     -> blur $ gauss 1
  BoxBlur       -> blur   box
  TriangleBlur  -> blur   triangle
  -- Quadratic -> undefined

data BlurType = GaussBlur | BoxBlur | TriangleBlur -- | Quadratic

--blurKernel :: Exp Int -> Exp Double -> Matrix2 Double
--blurKernel size sigma = normalize $ toMatrix (Grid size size) (gauss sigma)

--blurKernelV :: Exp Int -> Exp Double -> Matrix2 Double
--blurKernelV size sigma = normalize $ toMatrix (Grid size 1) (gauss sigma)

--blurKernelH :: Exp Int -> Exp Double -> Matrix2 Double
--blurKernelH size sigma = normalize $ toMatrix (Grid 1 size) (gauss sigma)

--blur size sigma img = applyKernel (blurKernelV size sigma) (applyKernel (blurKernelH size sigma) img) -- $ applyKernel $ blurKernelV size sigma
--blur :: Exp Int -> Exp Double -> CartesianShader (Exp Double) (Exp Double) -> CartesianShader (Exp Double) (Exp Double)
--blur size sigma img = nearest $ F.filter 1 (blurKernelV size sigma) $ F.filter 1 (blurKernelV size sigma) $ monosampler img

blur :: (IsFloating a, Elt a) => Filter (Exp a) -> Exp Int -> DiscreteShader (Exp a) -> DiscreteShader (Exp a)
blur kernel kernSize img = process img where
    hmat = id M.>-> normalize $ toMatrix (Grid 1 (variable kernSize)) kernel
    vmat = id M.>-> normalize $ toMatrix (Grid (variable kernSize) 1) kernel
    p = pipe A.Clamp
    process x = id `p` F.filter 1 vmat `p` F.filter 1 hmat `p` id $ x
