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
import qualified Luna.Syntax.Model.Graph.Cluster as Cluster
import qualified Data.GraphViz.Attributes.HTML as Html
import Data.Index (idx)

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

--instance Repr HeaderOnly Data where repr _ = "Data"
--instance Repr HeaderOnly (Draft l v) where repr _ = "Draft"

-- Skin definition

bgClr         = GVC.Gray12
gClr          = GVC.Gray30

typedArrClr   = GVC.Firebrick
namedArrClr   = GVC.Turquoise
accArrClr     = GVC.Yellow
arrClr        = GVC.DarkOrange

nodeClr       = HSV 0.6 0.64 0.95
valIntNodeClr = GVC.Chartreuse
valStrNodeClr = GVC.LimeGreen
valLitNodeClr = HSV 0.4 0.64 0.95
valUnkNodeClr = GVC.Red
dirtyClr      = GVC.MediumOrchid
checkedClr    = GVC.MediumOrchid

graphLabelClr = GVC.Gray30
nodeLabelClr  = GVC.Gray8
edgeLabelClr  = GVC.Gray40

portClr = HSV 0.0 0.0 0.3

fontName = "arial"
fontSize = 10.0


gStyle :: String -> [GlobalAttributes]
gStyle name = [ GraphAttrs [ RankDir FromTop
                           , Splines SplineEdges
                           , fontColor graphLabelClr
                           , FontName fontName
                           , FontSize fontSize
                           , bgColor  bgClr
                           , color    gClr
                           , GV.Label $ StrLabel $ fromString name
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




toGraphViz :: forall a. Show a => String -> NetGraph a -> DotGraph String
toGraphViz name net = DotGraph { strictGraph     = False
                               , directedGraph   = True
                               , graphID         = Nothing
                               , graphStatements = DotStmts { attrStmts = gStyle name
                                                            , subGraphs = subGraphs
                                                            , nodeStmts = nodeStmts
                                                            , edgeStmts = edgeStmts
                                                            }
                               }
    where -- === Inputs === --

          ng                = net ^. nodeGraph
          eg                = net ^. edgeGraph
          cg                = net ^. clusters
          nodeIxs           = usedIxes ng :: [Int]
          clrIxs            = usedIxes cg
          clrs              = elems $ net ^. clusters
          clredNodeIxs      = zip (safeHead ∘ matchClusters clrIxs <$> nodeIxs) nodeIxs :: [(Maybe Int, Int)]
          clredNodeMap      = fromListWithReps clredNodeIxs :: Map (Maybe Int) [Int]
          rootNodeIxs       = case Map.lookup Nothing clredNodeMap of
                                  Nothing  -> []
                                  Just ixs -> ixs


          -- === outputs === --

          inEdges           = concat $ fmap nodeInEdges nodeIxs
          edgeStmts         = fmap mkEdge inEdges
          nodeStmts         = labeledNode <$> rootNodeIxs
          subGraphs         = uncurry genSubGraph ∘ (_1 %~ fromJust) <$> Map.assocs (Map.delete Nothing clredNodeMap)


          -- === Utils === --

          inPortName num    = fromString $ "in" <> show num
          nodeRef         i = "<node " <> show i <> ">"

          --labeledNode ix    = DotNode ref attrs where
          --    ref    = nodeRef ix
          --    node   = draftNodeByIx ix
          --    label  = GV.Label ∘ StrLabel ∘ fromString $ genNodeLabel node
          --    colors = nodeColorAttr node
          --    attrs  = label : colors

          labeledNode ix    = DotNode ref attrs where
              ref    = nodeRef ix
              node   = draftNodeByIx ix
              ins    = node # Inputs

              inPortsNum = length ins
              inPorts    = port <$> [0 .. inPortsNum - 1]
              inLayout   = if length inPorts < 2 then [] else [Html.Cells inPorts]
              label      = GV.Label ∘ StrLabel ∘ fromString $ ""
              htmlCells  = Html.Cells [labelCell width $ fromString $ genNodeLabel node] where
                  width  = if null inPorts then 1 else fromIntegral inPortsNum

              labelCell cs s = Html.LabelCell [Html.ColSpan cs, Html.BGColor $ color, Html.Port $ fromString "label"] $ Html.Text [Html.Str $ fromString s]
              port num       = Html.LabelCell [Html.ColSpan 1, Html.Height 2, Html.Border 0, Html.Port $ inPortName num, Html.BGColor $ portClr] $ Html.Text [Html.Str $ fromString ""]
              htmlLabel      = GV.Label $ HtmlLabel $ Html.Table $ Html.HTable Nothing [Html.CellSpacing 3, Html.CellBorder 1, Html.Border 0] $ inLayout <> [htmlCells]
              color   = nodeColor node
              --attrs   = GV.color color : shAttrs
              attrs   = Color (toColorList [color]) : shAttrs
              shAttrs = caseTest (uncover node) $ do
                  match $ \(Term.Unify a b) -> [shape DoubleCircle, label, Width 0.2, Height 0.2]
                  match $ \ANY              -> [shape PlainText   , htmlLabel]

          nodeInEdges   n   = zip3 ([0..] :: [Int]) (genInEdges net $ (cast $ index n ng :: NetLayers a :< Draft Static)) (repeat n)
          mkEdge  (n,(a,attrs),b) = DotEdge (nodeRef a) (nodeRef b) $ HeadPort (LabelledPort (inPortName n) Nothing): TailPort (LabelledPort "label" Nothing) : attrs

          draftNodeByIx ix   = cast $ index_ ix ng :: (NetLayers a :< Draft Static)
          clusterByIx   ix   = index_ ix cg        :: Cluster
          genNodeLabel  node = reprStyled HeaderOnly $ uncover node

          matchCluster :: Int -> Int -> Maybe Int
          matchCluster clrIx  nodeIx = if Cluster.member nodeIx (clusterByIx clrIx) then Just clrIx else Nothing
          matchClusters :: [Int] -> Int -> [Int]
          matchClusters clrIxs nodeIx = catMaybes $ flip matchCluster nodeIx <$> clrIxs

          --nodeColor :: (NetLayers a :< Draft Static) -> Attribute
          nodeColor n = caseTest (uncover n) $ do
                                match $ \(Term.Str s) -> valLitNodeClr
                                match $ \(Term.Num n) -> valLitNodeClr
                                match $ \ANY          -> nodeClr

          genSubGraph :: Int -> [Int] -> DotSubGraph String
          genSubGraph sgIdx nodeIxs = DotSG
              { isCluster     = True
              , subGraphID    = Just $ Str $ fromString $ show sgIdx
              , subGraphStmts = DotStmts { attrStmts = gStyle ("Subgraph " <> show sgIdx)
                                         , subGraphs = []
                                         , nodeStmts = labeledNode <$> nodeIxs
                                         , edgeStmts = []
                                         }
              }



instance IsString PortName where fromString = PN . fromString

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (a:_) = Just a


fromListWithReps :: Ord k => [(k,v)] -> Map k [v]
fromListWithReps lst = foldr update (Map.fromList initLst) lst where
    ks           = fst   <$> lst
    initLst      = (,[]) <$> ks
    update (k,v) = Map.adjust (v:) k


genInEdges (g :: NetGraph a) (n :: NetLayers a :< Draft Static) = displayEdges where
    --displayEdges = tpEdge : (addColor <$> inEdges)
    displayEdges = ($ (addColor <$> inEdges)) $ if t == universe then id else (<> [tpEdge])
    genLabel     = GV.Label . StrLabel . fromString . show
    ins          = n # Inputs
    inIdxs       = getTgtIdx <$> ins
    inEdges      = zipWith (,) inIdxs $ fmap ((:[]) . genLabel) [0..]
    es           = g ^. edgeGraph
    te           = n ^. prop Type
    t            = getTgt te
    tpEdge       = (getTgtIdx te, [GV.color typedArrClr, ArrowHead dotArrow])

    addColor (idx, attrs) = (idx, GV.color arrClr : attrs)
    getTgtIdx             = view rawPtr ∘ getTgt
    getTgt    inp         = view source $ index (inp ^. rawPtr) es






universe = Ref $ Ptr 0 -- FIXME [WD]: Implement it in safe way. Maybe "star" should always result in the top one?















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


-- === Utils === --

renderAndOpen lst = do
    flip mapM_ lst $ \(name, g) -> render name $ toGraphViz name g
    open $ fmap (\s -> "/tmp/" <> s <> ".png") (reverse $ fmap fst lst)
