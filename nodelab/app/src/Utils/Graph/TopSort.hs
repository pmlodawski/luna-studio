module Utils.Graph.TopSort (topsort) where

import           Utils.PreludePlus
import qualified Data.Map          as Map
import           Data.Map          (Map)
import           Utils.Graph.Graph (Graph(..), neighbourhood, transposeGraph)
import           Control.Monad.State

data TopSortState a = TopSortState { _graph   :: Graph a
                                   , _orders  :: Map a Int
                                   , _sources :: [a]
                                   , _result  :: [a]
                                   } deriving (Show, Eq)
makeLenses ''TopSortState

initOrders :: Ord a => Graph a -> Map a Int
initOrders (Graph m) = Map.map length m

initState :: Ord a => Graph a -> TopSortState a
initState graph = TopSortState graph ords srcs [] where
    ords = initOrders $ transposeGraph graph
    srcs = Map.keys $ Map.filter (== 0) ords

topSortRec :: Ord a => State (TopSortState a) ()
topSortRec = do
    toProcess <- use sources
    case toProcess of
        (x : xs) -> do
            sources .= xs
            processNode x
            topSortRec
        _        -> return ()

processNode :: Ord a => a -> State (TopSortState a) ()
processNode node = do
    result %= (node :)
    neighs <- use $ graph . neighbourhood . at node . non []
    mapM_ decreaseOrd neighs

decreaseOrd :: Ord a => a -> State (TopSortState a) ()
decreaseOrd node = do
    currentOrd <- use $ orders . at node
    orders . at node .= (subtract 1 <$> currentOrd)
    if currentOrd == (Just 1)
        then sources %= (node :)
        else return ()

topsort :: (Show a, Ord a) => Graph a -> [a]
topsort graph = reverse res where
    TopSortState _ _ _ res = execState topSortRec $ initState graph
