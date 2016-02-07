{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- {-# LANGUAGE PartialTypeSignatures #-}

module Luna.Diagnostic.Vis.GraphViz where

import           Prelude.Luna                           hiding (index)

import           Data.GraphViz
import qualified Data.GraphViz.Attributes               as GV
import qualified Data.GraphViz.Attributes.Colors        as GVC
import qualified Data.GraphViz.Attributes.Colors.X11    as GVC
import           Data.GraphViz.Attributes.Complete      hiding (Int, Label, Star)
import qualified Data.GraphViz.Attributes.Complete      as GV
import           Data.GraphViz.Commands
import           Data.GraphViz.Printing                 (toDot)
import           Data.GraphViz.Printing                 (PrintDot)
import           Data.GraphViz.Types.Canonical
import           Luna.Syntax.Repr.Styles                (HeaderOnly (..), Simple (..))

import           Data.Container
import           Data.Container.Hetero

import           Data.Record
import           Luna.Syntax.Model.Graph
import           Luna.Syntax.Model.Network.Builder

import           Data.Container.Class
import           Data.Reprx
import           System.Platform
import           System.Process                         (createProcess, shell)

import           Data.Layer.Cover                       (uncover)
import           Data.Prop
import           Luna.Evaluation.Runtime                (Dynamic, Static)
import qualified Luna.Syntax.AST.Term                   as Term
import           Luna.Syntax.Model.Graph
import           Luna.Syntax.Model.Layer
import           Luna.Syntax.Model.Network.Builder.Term
import           Luna.Syntax.Model.Network.Term

--instance Repr HeaderOnly Data where repr _ = "Data"
--instance Repr HeaderOnly (Draft l v) where repr _ = "Draft"

-- Skin definition

bgClr         = GVC.Gray12

typedArrClr   = GVC.Firebrick
namedArrClr   = GVC.Turquoise
accArrClr     = GVC.Yellow
arrClr        = GVC.DarkOrange

nodeClr       = GVC.DeepSkyBlue
valIntNodeClr = GVC.Chartreuse
valStrNodeClr = GVC.LimeGreen
valUnkNodeClr = GVC.Red
dirtyClr      = GVC.MediumOrchid
checkedClr    = GVC.MediumOrchid

nodeLabelClr  = GVC.Gray75
edgeLabelClr  = GVC.Gray40



fontName = "arial"
fontSize = 10.0


gStyle :: [GlobalAttributes]
gStyle = [ GraphAttrs [ RankDir FromTop
                      , Splines SplineEdges
                      , FontName fontName
                      , bgColor  bgClr
                      ]
         , NodeAttrs  [ fontColor nodeLabelClr
                      , FontName fontName
                      , FontSize fontSize
                      ]
         , EdgeAttrs  [ fontColor edgeLabelClr
                      , FontName fontName
                      , FontSize fontSize
                      ]
         ]



labelAttrs = const []
--class LabelAttrs a where
--    labelAttrs :: a -> [GV.Attribute]

--instance

--instance LabelAttrs (WithMeta a (Labeled2 b (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge))))))) where
--    labelAttrs = const []

--instance LabelAttrs (Labeled2 b (Typed (Ref Edge) (SuccTracking (Coat (Draft (Ref Edge)))))) where
--    labelAttrs = const []

toGraphViz :: forall a. Show a => NetGraph a -> DotGraph String
toGraphViz net = DotGraph { strictGraph     = False
                          , directedGraph   = True
                          , graphID         = Nothing
                          , graphStatements = DotStmts { attrStmts = gStyle
                                                       , subGraphs = []
                                                       , nodeStmts = nodeStmts
                                                       , edgeStmts = edgeStmts
                                                       }
                          }
    where ng                = net ^. nodeGraph
          eg                = net ^. edgeGraph
          nodesE            = elems ng
          edgesE            = elems eg
          nodes'            = cast <$> nodesE :: [NetLayers a :< Draft Static]
          edges'            = cast <$> edgesE :: [Link (NetLayers a :< Draft Static)]
          nodeIds           = usedIxes ng
          nodeLabels        = reprStyled HeaderOnly . uncover <$> nodes'
          nodeStmts         = fmap (uncurry labeledNode) $ zip3 nodes' nodeLabels nodeIds
          inEdges           = concat $ fmap nodeInEdges nodeIds

          mkEdgeDesc (Edge src dst) = DotEdge (nodeRef $ src ^. rawPtr) (nodeRef $ dst ^. rawPtr) [GV.color arrClr]

          --edgeStmts         = mkEdgeDesc <$> edges'
          --inEdges           = zip3 ([0..] :: [Int])
          edgeStmts         = fmap mkEdge inEdges
          nodeRef         i = "<node " <> show i <> ">"
          labeledNode n s a = DotNode (nodeRef a) $ (GV.Label . StrLabel $ fromString s) : (nodeColorAttrs n) ++ labelAttrs n
          nodeInEdges   n   = zip3 ([0..] :: [Int]) (genInEdges net $ (cast $ index n ng :: NetLayers a :< Draft Static)) (repeat n)
          mkEdge  (n,(a,attrs),b) = DotEdge (nodeRef a) (nodeRef b) attrs -- (GV.edgeEnds Back : attrs)

--          allEdges        = drawEdge <$> elems_ eg

--          nodeColorAttrs n = case' (uncoat n) $ do
--                                match $ \(Val val :: Val (Ref Edge)) ->
--                                     case' val $ match $ \lit -> case lit of
--                                        Lit.Int    i -> [GV.color valIntNodeClr]
--                                        Lit.String s -> [GV.color valStrNodeClr]
--                                        _            -> [GV.color valUnkNodeClr]
--                                match $ \ANY         -> [GV.color nodeClr]

          nodeColorAttrs n = caseTest (uncover n) $ do
                                --match $ \(Val val :: Val (Ref Edge)) ->
                                --     case' val $ match $ \lit -> case lit of
                                --        Lit.Int    i -> [GV.color valIntNodeClr]
                                --        Lit.String s -> [GV.color valStrNodeClr]
                                --        _            -> [GV.color valUnkNodeClr]
                                match $ \ANY         -> [GV.color nodeClr]


genInEdges (g :: NetGraph a) (n :: NetLayers a :< Draft Static) = tpEdge : fmap addColor inEdges  where
    genLabel  = GV.Label . StrLabel . fromString . show
    ins       = n # Inputs
    inIdxs    = getTgtIdx <$> ins
    inEdges   = zipWith (,) inIdxs $ fmap ((:[]) . genLabel) [0..]
    es        = g ^. edgeGraph
    t         = n ^. prop Type
    tpEdge    = (getTgtIdx t, [GV.color typedArrClr, ArrowHead dotArrow])

    addColor (idx, attrs) = (idx, GV.color arrClr : attrs)
    getTgtIdx inp         = view (source ∘ rawPtr) $ index (inp ^. rawPtr) es

    --getIdx  i = deref . view target $ index (deref i) eg
    --n =


----          drawEdge (DoubleArc start end) = DotEdge (nodeRef $ deref start) (nodeRef $ deref end) []

--class GenInEdges n e a where
--    genInEdges :: Graph n e -> a -> [(Int, [GV.Attribute])]

----instance GenInEdges n e a => GenInEdges n e (Labeled2 l a) where
----    genInEdges g (Labeled2 _ a) = genInEdges g a

----instance GenInEdges n DoubleArc a => GenInEdges n DoubleArc (Typed (Ref Edge) a) where
----    genInEdges g (Typed t a) = [(tgt, [GV.color typedArrClr, ArrowHead dotArrow])] <> genInEdges g a where
----        tgt = deref . view target $ index (deref t) (g ^. edges)

----instance GenInEdges n e a => GenInEdges n e (SuccTracking a) where
----    genInEdges g = genInEdges g . unlayer

----instance GenInEdges n e a => GenInEdges n e (Coat a) where
----    genInEdges g = genInEdges g . unwrap

----instance GenInEdges n DoubleArc (Draft (Ref Edge)) where
----    genInEdges g a = ($ inEdges) $ case checkName a of
----        Nothing -> fmap addColor
----            where addColor (idx, attrs) = (idx, GV.color arrClr : attrs)
----        Just  t -> fmap addColor
----            where tidx = getIdx t
----                  addColor (idx, attrs) = if idx == tidx then (idx, GV.color namedArrClr : attrs)
----                                                         else (idx, GV.color accArrClr   : attrs)
----        where genLabel  = GV.Label . StrLabel . fromString . show
----              ins       = inputs a
----              getIdx  i = deref . view target $ index (deref i) edges'
----              inIdxs    = getIdx <$> ins
----              inEdges   = zipWith (,) inIdxs $ fmap ((:[]) . genLabel) [0..]
----              edges'    = g ^. edges
----              xtt       = case' a $ do
----                              match $ \(Val _) -> 11

----instance GenInEdges n e a => GenInEdges n e (WithMeta b a) where
----    genInEdges g = genInEdges g . view node


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
