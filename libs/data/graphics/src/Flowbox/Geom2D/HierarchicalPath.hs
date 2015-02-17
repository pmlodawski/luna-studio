module Flowbox.Geom2D.HierarchicalPath where

import qualified Flowbox.Geom2D.ControlPoint 			as C
import qualified Flowbox.Geom2D.Path 		 			as P
import qualified Flowbox.Geom2D.Mask 		 			as M
import 			 Flowbox.Prelude 			 			as P
import qualified Flowbox.Graphics.Composition.Transform as Transform

import           Linear                    				(V2 (..))
import           Math.Coordinate.Cartesian
import 			 Data.IntMap 							as I
import			 Data.Vector 							as V
import			 Data.Maybe

type Rank      = Int
type Angles a  = (a, a)
type Handles a = (V2 a, V2 a)

data ControlPoint a = BasePoint 	 { coords  :: Point2 a
									 , handles :: Handles a
									 }
					| DependentPoint { rank	   :: Rank
									 , angles  :: Angles a
									 , handles :: Handles a
									 } deriving (Eq, Ord, Show)

data Path a = Path { isClosed      :: Bool
                   , controlPoints :: [ControlPoint a]
                   } deriving (Eq, Ord, Show)

data OrdinaryControlPoint a = OrdinaryControlPoint { controlPoint :: Point2 a
                                   , handleIn     :: Maybe (Point2 a)
                                   , handleOut    :: Maybe (Point2 a)
                                   } deriving (Eq, Ord, Show)

type Parents   = (Maybe Int, Maybe Int)

makeTree :: [ControlPoint a] -> IntMap Parents
makeTree points = {-aux 0 (ranks ++ ranks) stacks pred-} (I.fromList $ genList (Nothing, Nothing) len)
	where
		aux :: Int -> [Int] -> IntMap [Int] -> IntMap (Maybe Int) -> IntMap Parents -> IntMap Parents
		aux idx [] stacks pred acc = acc
		aux idx (0:l) stacks pred acc = aux (idx+1) l stacks pred acc
		aux idx (rank:l) stacks pred acc = aux (idx+1) l stacks' pred' acc'
			where 
				idx'    = idx `mod` len
				pred'   = I.insert rank (Just idx') pred
				deps    = stacks I.! (rank+1)
				stacks' = I.insert (rank+1) ([]) $ I.adjust (\x -> (idx':x)) rank stacks
				(left,right)   = acc I.! idx'
				left'      = pred I.! (rank-1)
				acc'    = updateDeps idx deps $ I.insert idx (left',right) acc

				updateDeps :: Int -> [Int] -> IntMap Parents -> IntMap Parents
				updateDeps idx [] acc = acc
				updateDeps idx (a:l) acc = updateDeps idx l acc'
					where
						(left,right) = acc I.! a
						acc' = I.insert a (left, Just idx) acc

		len    = P.length points
		stacks = I.fromList $ genList ([]) (maxRank+1)
		pred   = I.fromList $ genList (Nothing) (maxRank+1)
		ranks = P.map extract points
		maxRank = P.maximum ranks

		extract :: ControlPoint a -> Int
		extract x = 
			case x of
				BasePoint _ _ -> 0
				DependentPoint r _ _ -> r

		genList :: a -> Int -> [(Int,a)]
		genList v len = P.take len $ P.map (\x -> (x, v)) $ P.iterate (+1) 0


