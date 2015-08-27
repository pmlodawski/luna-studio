{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Diagnostic.AST where

import Prologue

import           Data.GraphViz.Types.Canonical
import           Data.GraphViz.Attributes.Complete   hiding (Label, Int)
import qualified Data.GraphViz.Attributes.Complete   as GV
import qualified Data.GraphViz.Attributes            as GV
import           Data.GraphViz.Printing              (toDot)
import           Data.GraphViz.Commands
import qualified Data.GraphViz.Attributes.Colors     as GVC
import qualified Data.GraphViz.Attributes.Colors.X11 as GVC
import           Data.GraphViz.Printing              (PrintDot)
import           Luna.Repr.Styles (HeaderOnly(..), Simple(..))

import Data.Cata
import Data.Containers
import Data.Containers.Hetero

import Luna.Syntax.AST.Term
import Luna.Syntax.Builder.Graph
import Luna.Syntax.AST
import Luna.Syntax.Name
import Luna.Syntax.Layer.Typed
import Luna.Syntax.Layer.Labeled

import System.Process (createProcess, shell)

toGraphViz :: _ => HomoGraph ArcPtr a -> DotGraph Int
toGraphViz g = DotGraph { strictGraph     = False
                        , directedGraph   = True
                        , graphID         = Nothing
                        , graphStatements = DotStmts { attrStmts = []
                                                     , subGraphs = []
                                                     , nodeStmts = nodeStmts
                                                     , edgeStmts = edgeStmts
                                                     }
                        }
    where nodes           = elems g
          nodeIds         = indexes g
          nodeLabels      = fmap (reprStyled HeaderOnly . view ast) nodes
          labeledNode s a = DotNode a [GV.Label . StrLabel $ fromString s]
          nodeStmts       = fmap (uncurry labeledNode) $ zip nodeLabels nodeIds
          nodeInEdges   n = zip3 [0..] (genEdges $ unsafeIndex n g) (repeat n)
          inEdges         = concat $ fmap nodeInEdges nodeIds
          mkEdge  (n,(a,attrs),b) = DotEdge a b attrs
          edgeStmts       = fmap mkEdge inEdges


class GenEdges a where
    genEdges :: a -> [(Int, [GV.Attribute])]

instance GenEdges (a t) => GenEdges (Labeled l a t) where
    genEdges (Labeled _ a) = genEdges a

instance (t ~ Mu (Ref Int a), GenEdges (Term t)) => GenEdges (Typed Term t) where
    genEdges (Typed t a) = [(ptrIdx . fromRef . fromMu $ t, [GV.color GVC.Red])] <> genEdges a

instance t ~ Mu (Ref Int a) => GenEdges (Term t) where
    genEdges a = ($ inEdges) $ case checkName a of
        Nothing -> id
        Just  t -> fmap addColor
            where tidx = getIdx t
                  addColor (idx, attrs) = if idx == tidx then (idx, GV.color GVC.Blue : attrs)
                                                         else (idx, attrs)
        where genLabel  = GV.Label . StrLabel . fromString . show
              ins       = inputs a
              getIdx    = ptrIdx . view content
              inIdxs    = getIdx <$> ins
              inEdges   = zipWith (,) inIdxs $ fmap ((:[]) . genLabel) [0..]



class Displayable m a where
    display :: a -> m ()

instance (MonadIO m, Ord a, PrintDot a) => Displayable m (DotGraph a) where
    display gv = do
        liftIO $ runGraphviz gv Png "/tmp/out.png"
        liftIO $ createProcess $ shell "open /tmp/out.png"
        return ()