{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Diagnostic.AST where

import Prologue  hiding (index)

import           Data.GraphViz.Types.Canonical
import           Data.GraphViz.Attributes.Complete   hiding (Label, Int, Star)
import qualified Data.GraphViz.Attributes.Complete   as GV
import qualified Data.GraphViz.Attributes            as GV
import           Data.GraphViz.Printing              (toDot)
import           Data.GraphViz.Commands
import qualified Data.GraphViz.Attributes.Colors     as GVC
import qualified Data.GraphViz.Attributes.Colors.X11 as GVC
import           Data.GraphViz.Printing              (PrintDot)
import           Luna.Repr.Styles (HeaderOnly(..), Simple(..))

import Data.Cata
import Data.Container
import Data.Container.Hetero

import Luna.Syntax.AST.Term
import Luna.Syntax.Repr.Graph
import Luna.Syntax.AST
import Luna.Syntax.AST.Typed
import Luna.Syntax.Name
import Luna.Syntax.Layer.Labeled

import System.Platform
import System.Process (createProcess, shell)
import Data.Container.Class
import Data.Reprx

import Data.Layer.Coat

import Data.Variants


--toGraphViz :: _ => HomoGraph ArcPtr a -> DotGraph Int
--toGraphViz g = undefined
--toGraphViz g = DotGraph { strictGraph     = False
--                        , directedGraph   = True
--                        , graphID         = Nothing
--                        , graphStatements = DotStmts { attrStmts = []
--                                                     , subGraphs = []
--                                                     , nodeStmts = nodeStmts
--                                                     , edgeStmts = edgeStmts
--                                                     }
--                        }
--    where nodes           = elems g
--          nodeIds         = usedIxes g
--          nodeLabels      = fmap (reprStyled HeaderOnly . view ast) nodes
--          labeledNode s a = DotNode a [GV.Label . StrLabel $ fromString s]
--          nodeStmts       = fmap (uncurry labeledNode) $ zip nodeLabels nodeIds
--          nodeInEdges   n = zip3 ([0..] :: [Int]) (genEdges $ index n g) (repeat n)
--          inEdges         = concat $ fmap nodeInEdges nodeIds
--          mkEdge  (n,(a,attrs),b) = DotEdge a b attrs -- (GV.edgeEnds Back : attrs)
--          edgeStmts       = fmap mkEdge inEdges


toGraphViz :: _ => Graph n e -> DotGraph Int
toGraphViz net = DotGraph { strictGraph     = False
                          , directedGraph   = True
                          , graphID         = Nothing
                          , graphStatements = DotStmts { attrStmts = []
                                                       , subGraphs = []
                                                       , nodeStmts = nodeStmts
                                                       , edgeStmts = edgeStmts
                                                       }
                          }
    where g               = net ^. nodes
          edges'          = net ^. edges
          nodes'          = elems g
          nodeIds         = usedIxes g
          nodeLabels      = fmap (reprStyled HeaderOnly . uncoat) nodes'
          labeledNode n s a = DotNode a $ (GV.Label . StrLabel $ fromString s) : (nodeColorAttrs $ uncoat n)
          nodeStmts       = fmap (uncurry labeledNode) $ zip3 nodes' nodeLabels nodeIds
          nodeInEdges   n = zip3 ([0..] :: [Int]) (genEdges net $ index n g) (repeat n)
          inEdges         = concat $ fmap nodeInEdges nodeIds
          mkEdge  (n,(a,attrs),b) = DotEdge a b attrs -- (GV.edgeEnds Back : attrs)
          edgeStmts       = fmap mkEdge inEdges

          nodeColorAttrs a = case' a $ do
                                 match $ \(Val _ :: Val (Ref Edge)) -> [GV.color GVC.Green]
                                 match $ \ANY                       -> []

class GenEdges n e a where
    genEdges :: Graph n e -> a -> [(Int, [GV.Attribute])]

instance GenEdges n e a => GenEdges n e (Labeled2 l a) where
    genEdges g (Labeled2 _ a) = genEdges g a

instance GenEdges n DoubleArc a => GenEdges n DoubleArc (Typed (Ref Edge) a) where
    genEdges g (Typed t a) = [(tgt, [GV.color GVC.Red, GV.edgeEnds Back])] <> genEdges g a where
        tgt = deref . view target $ index (deref t) (g ^. edges)

instance GenEdges n e a => GenEdges n e (SuccTracking a) where
    genEdges g = genEdges g . unlayer

instance GenEdges n e a => GenEdges n e (Coat a) where
    genEdges g = genEdges g . unwrap

instance GenEdges n DoubleArc (Draft (Ref Edge)) where
    genEdges g a = ($ inEdges) $ case checkName a of
        Nothing -> id
        Just  t -> fmap addColor
            where tidx = getIdx t
                  addColor (idx, attrs) = if idx == tidx then (idx, GV.color GVC.Blue : attrs)
                                                         else (idx, attrs)
        where genLabel  = GV.Label . StrLabel . fromString . show
              ins       = inputs a
              getIdx  i = deref . view target $ index (deref i) edges'
              inIdxs    = getIdx <$> ins
              inEdges   = zipWith (,) inIdxs $ fmap ((:[]) . genLabel) [0..]
              edges'    = g ^. edges
              xtt       = case' a $ do
                              match $ \(Val _) -> 11



class Displayable m a where
    render  :: String -> a -> m ()
    display :: a -> m ()

class OpenUtility p where
    openUtility :: MonadIO m => p -> [FilePath] -> m ()

instance OpenUtility Windows where openUtility = const $ singleProcess "start"
instance OpenUtility Darwin  where openUtility = const $ singleProcess "open"
instance OpenUtility Linux   where openUtility = const $ manyProcess   "xdg-open"
instance OpenUtility GHCJS   where openUtility = const $ singleProcess "open"

singleProcess, manyProcess :: MonadIO m => String -> [FilePath] -> m ()
singleProcess p args = liftIO $ void $ createProcess $ shell $ p <> " " <> intercalate " " args
manyProcess   p = liftIO . mapM_ (\a -> createProcess $ shell $ p <> " " <> a)

open paths = openUtility platform paths


instance (MonadIO m, Ord a, PrintDot a) => Displayable m (DotGraph a) where
    render name gv = do
        let path = "/tmp/" <> name <> ".png"
        liftIO $ runGraphviz gv Png path
        return ()

    display gv = do
        let path = "/tmp/out.png"
        liftIO $ runGraphviz gv Png path
        open [path]
        return ()
