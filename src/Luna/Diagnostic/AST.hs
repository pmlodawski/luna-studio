{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Luna.Diagnostic.AST where

import Prologue  hiding (index)

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
import Data.Container
import Data.Container.Hetero

import Luna.Syntax.AST.Term
import Luna.Syntax.Builder.Graph
import Luna.Syntax.AST
import Luna.Syntax.Name
import Luna.Syntax.Layer.Typed
import Luna.Syntax.Layer.Labeled

import System.Platform
import System.Process (createProcess, shell)
import Data.Container.Class
import Data.Reprx

toGraphViz :: _ => HomoGraph ArcPtr a -> DotGraph Int
--toGraphViz g = undefined
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
          nodeIds         = usedIxes g
          nodeLabels      = fmap (reprStyled HeaderOnly . view ast) nodes
          labeledNode s a = DotNode a [GV.Label . StrLabel $ fromString s]
          nodeStmts       = fmap (uncurry labeledNode) $ zip nodeLabels nodeIds
          nodeInEdges   n = zip3 ([0..] :: [Int]) (genEdges $ index n g) (repeat n)
          inEdges         = concat $ fmap nodeInEdges nodeIds
          mkEdge  (n,(a,attrs),b) = DotEdge a b attrs -- (GV.edgeEnds Back : attrs)
          edgeStmts       = fmap mkEdge inEdges


class GenEdges a where
    genEdges :: a -> [(Int, [GV.Attribute])]

instance GenEdges (a t) => GenEdges (Labeled l a t) where
    genEdges (Labeled _ a) = genEdges a

instance (t ~ Mu (Ref Int a), GenEdges (Draft t)) => GenEdges (Typed Draft t) where
    genEdges (Typed t a) = [(ptrIdx . fromRef . unwrap $ t, [GV.color GVC.Red, GV.edgeEnds Back])] <> genEdges a

instance t ~ Mu (Ref Int a) => GenEdges (Draft t) where
    genEdges a = ($ inEdges) $ case checkName a of
        Nothing -> id
        Just  t -> fmap addColor
            where tidx = getIdx t
                  addColor (idx, attrs) = if idx == tidx then (idx, GV.color GVC.Blue : attrs)
                                                         else (idx, attrs)
        where genLabel  = GV.Label . StrLabel . fromString . show
              ins       = inputs a
              getIdx    = ptrIdx . fromRef . unwrap
              inIdxs    = getIdx <$> ins
              inEdges   = zipWith (,) inIdxs $ fmap ((:[]) . genLabel) [0..]



class Displayable m a where
    render  :: String -> a -> m ()
    display :: a -> m ()

class OpenUtility p where
    openUtility :: p -> String

instance OpenUtility Windows where openUtility = const "start"
instance OpenUtility Darwin  where openUtility = const "open"
instance OpenUtility Linux   where openUtility = const "xdg-open"
instance OpenUtility GHCJS   where openUtility = const "open"

open paths = liftIO . createProcess . shell $ openUtility platform <> " " <> mjoin " " paths

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
