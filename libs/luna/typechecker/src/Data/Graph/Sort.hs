{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Sort where

import           Control.Monad.Extra        (whenJustM)
import           Control.Monad.Identity
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Container.Class
import qualified Data.List                  as List
import           Prologue



class (Graph g, ForwardEdgedGraph g, MarkableGraph g, Eq (Item g)) => CompleteGraph g

class Graph g where
    listNodes :: g -> [Item g]

class BackEdgedGraph g where
    predecessors :: Item g -> g -> [Item g]

class ForwardEdgedGraph g where
    successors :: Item g -> g -> [Item g]

class MarkableGraph g where
    markNode :: Item g -> g -> g
    isMarked :: Item g -> g -> Bool

class SortBy g where
    sortBy :: (Item g -> Bool) -> g -> Either SortError [Item g]

data SortError = GraphContainsCycle


data SortState g = SortState
        { _graph  :: g
        , _sorted :: [Item g]
        }

makeLenses ''SortState

instance CompleteGraph g => SortBy g where
    sortBy predicate g = runIdentity $ runExceptT $ view sorted <$> execStateT sortBy' (SortState g []) where
        sortBy' = whenJustM notMarkedNode $ \selected ->
                        visit selected >> sortBy'
        notMarkedNode = List.find predicate . listNodes <$> use graph
        visit node = do
            ifM (isMarked node <$> use graph)
                (lift $ throwE GraphContainsCycle) $
                do  graph %= markNode node
                    mapM_ visit . successors node =<< use graph
                    sorted %= (node:)
