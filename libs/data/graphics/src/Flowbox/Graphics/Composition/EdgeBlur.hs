

module Flowbox.Graphics.Composition.EdgeBlur where

import           Flowbox.Graphics.Composition.Filter as F
--import           Data.Array.Accelerate               as A hiding (constant, filter, scatter, size, stencil)
import           Data.Array.Accelerate.CUDA          as A
--import             Flowbox.Graphics.Composition.Generator.Shape
--import             Flowbox.Graphics.Shader.Matrix
--import             Flowbox.Graphics.Shader.Shader
--import           Flowbox.Math.Matrix                 as M hiding (size, stencil)
import           Flowbox.Graphics.Shader.Sampler
import           Flowbox.Graphics.Shader.Rasterizer
--import             Flowbox.Graphics.Shader.Stencil
import Prelude


import Flowbox.Graphics.Utils.Utils
import Flowbox.Graphics.Shader.Pipe
import Flowbox.Graphics.Composition.Generator.Shape
import Flowbox.Graphics.Shader.Matrix
import Flowbox.Graphics.Shader.Shader
import Flowbox.Graphics.Shader.Stencil

import           Flowbox.Graphics.Prelude            as P hiding (filter)
import           Flowbox.Math.Matrix                 as M hiding (size, stencil)
import qualified Flowbox.Math.Matrix                 as M (stencil)
import           Flowbox.Math.BitonicSorterGenerator as B
import           Data.Array.Accelerate               as A hiding (constant, filter, scatter, size, stencil)

import Math.Space.Space
import Math.Coordinate.Cartesian (Point2(..))



applyKernel :: (IsNum a, Elt a) => Matrix2 a -> CartesianShader (Exp Float) (Exp a) -> CartesianShader (Exp Float) (Exp a)
applyKernel kernel = nearest P.. convolve mode kernel
    where fi = fmap A.fromIntegral
          mode point offset = fi point + fi offset

-- bluredImg = blur n sigma testShader

mixImages edges first second = (+) <$> 
                               ( (*) <$> first <*> edges ) 
                               <*> 
                               ( (*) <$> second <*> (fmap ((-) 1) edges) )


detectEdges img = (\x y ->  P.min 1.0 $  2*x+2*y) <$> -- coeficients???
                  (fmap abs $ applyKernel (M.transpose (sobel :: Matrix2 Float)) img) 
                  <*> 
                  (fmap abs $ applyKernel (sobel :: Matrix2 Float) img)

--processImage img = (blur n sigma) (detectEdges img)

--edgeBlur img = mixImages (processImage img) img (blur n sigma img)

--matEdgeBlur matImg mask = rasterizer $ monosampler $ edgeBlur (nearest $ fromMatrix Clamp matImg)

edges mat = rasterizer $ monosampler $ blur 25 5.0 $ nearest $ dilate (Grid 10 10)  $ erode (Grid 3 3) $ monosampler  $ detectEdges (blur 10 2.0 $ nearest $ fromMatrix Clamp mat)

matBlur size sigma mat = rasterizer $ monosampler $ blur size sigma (nearest $ fromMatrix Clamp mat)

-- eedgeBBlur matteChannel img = forAllChannel img (matEdgeBlur mask)
--                               where mask = edges matteChannel



ebTop :: String -> Exp Int -> Exp Float -> Matrix2 Float -> [Matrix2 Float] -> [Matrix2 Float]
ebTop blurType size sigma matteCh chs =
  let blur = edgeBlur sigma size matteCh
    in fmap blur chs

ebTop' :: String -> Exp Int -> Exp Float -> Matrix2 Float -> [Matrix2 Float] -> [Matrix2 Float]
ebTop' blurType size sigma matteCh chs =
  let blur = edgeBlur' sigma size matteCh
    in fmap blur chs


edgeBlur :: Exp Float -> Exp Int -> Matrix2 Float -> Matrix2 Float -> Matrix2 Float
edgeBlur sigma size mask img = 
  let imgShader = nearest $ fromMatrix Clamp img
      maskEdges =  blur 15 5.0 $  nearest $  dilate (Grid 10 10) $ {-- erode (Grid 3 3) $ --} monosampler $  detectEdges $  nearest $ fromMatrix Clamp mask
      blured = blur size sigma imgShader
      result = mixImages maskEdges blured imgShader
    in rasterizer $ monosampler $ result

edgeBlur' :: Exp Float -> Exp Int -> Matrix2 Float -> Matrix2 Float -> Matrix2 Float
edgeBlur' sigma size mask img = 
  let imgShader = nearest $ fromMatrix Clamp img
      maskEdges =  blur 15 5.0 $ nearest $ dilate (Grid 10 10) $ erode (Grid 3 3) $ monosampler $ detectEdges $ nearest $ fromMatrix Clamp mask
      blured = blur size sigma imgShader
      result = mixImages maskEdges blured imgShader
    in rasterizer $ monosampler $ result


blurKernel :: Exp Int -> Exp Float -> Matrix2 Float
blurKernel size sigma = normalize $ toMatrix (Grid size size) (gauss sigma) 

blurKernelV :: Exp Int -> Exp Float -> Matrix2 Float
blurKernelV size sigma = normalize $ toMatrix (Grid size 1) (gauss sigma) 

blurKernelH :: Exp Int -> Exp Float -> Matrix2 Float
blurKernelH size sigma = normalize $ toMatrix (Grid 1 size) (gauss sigma) 









--blur size sigma img = applyKernel (blurKernelV size sigma) (applyKernel (blurKernelH size sigma) img) -- $ applyKernel $ blurKernelV size sigma
--blur :: Exp Int -> Exp Float -> CartesianShader (Exp Float) (Exp Float) -> CartesianShader (Exp Float) (Exp Float)
--blur size sigma img = nearest $ F.filter 1 (blurKernelV size sigma) $ F.filter 1 (blurKernelV size sigma) $ monosampler img

blur :: Exp Int -> Exp Float -> CartesianShader (Exp Float) (Exp Float) -> CartesianShader (Exp Float) (Exp Float)
blur kernSize sigma img = 
  let hmat = id M.>-> normalize $ toMatrix (Grid 1 (variable kernSize)) $ gauss sigma
      vmat = id M.>-> normalize $ toMatrix (Grid (variable kernSize) 1) $ gauss sigma
      p = pipe A.Clamp
      process x = id `p` F.filter 1 vmat `p` F.filter 1 hmat `p` id $ x
    in nearest $ process $ monosampler img

--testMat :: Matrix2 Float
--testMat = M.fromList (Z :. 4 :. 4) [  0, 0, 0, 0
--                                    , 0, 1, 1, 0
--                                    , 0, 1, 1, 0
--                                    , 0, 0, 0, 0
--                                   ]

--testShader :: CartesianShader (Exp Float) (Exp Float)
--testShader = nearest $ fromMatrix Clamp testMat

--n :: Exp Int
--n = 5

--sigma :: Exp Float
--sigma = 1

--test = do
--    Prelude.putStrLn "testShader"
--    Prelude.print $ M.toList A.run $ rasterizer $ monosampler $ testShader
--    Prelude.putStrLn "detectEdges"
--    Prelude.print $ M.toList A.run $ rasterizer $ monosampler $ detectEdges testShader
--    --putStrLn "sobel"
--    --print $ show (sobel :: Matrix2 Float)

--    Prelude.putStrLn "blur"
--    Prelude.print $ M.toList A.run $ rasterizer $ monosampler $ blur n sigma testShader

--    Prelude.putStrLn "edges blured"
--    Prelude.print $ M.toList A.run $ rasterizer $ monosampler $ processImage testShader

--    Prelude.putStrLn "mixing test"
--    Prelude.print $ M.toList A.run $ rasterizer $ monosampler $ mixImages (processImage testShader) testShader bluredImg 
