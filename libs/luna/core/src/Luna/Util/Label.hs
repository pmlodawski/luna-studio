---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Util.Label where

import           Flowbox.Prelude                  hiding (cons)
import           Luna.Syntax.Arg                  (Arg)
import qualified Luna.Syntax.Arg                  as A
import           Luna.Syntax.Decl                 (LDecl)
import qualified Luna.Syntax.Decl                 as Decl
import           Luna.Syntax.Expr                 (LExpr)
import qualified Luna.Syntax.Expr                 as Expr
import           Luna.Syntax.Foreign              (Foreign (Foreign))
import           Luna.Syntax.Graph.DefaultsMap    (DefaultsMap)
import qualified Luna.Syntax.Graph.DefaultsMap    as DefaultsMap
import           Luna.Syntax.Graph.Graph          (Graph)
import qualified Luna.Syntax.Graph.Graph          as Graph
import           Luna.Syntax.Graph.Node           (Node)
import qualified Luna.Syntax.Graph.Node           as Node
import           Luna.Syntax.Graph.Node.Expr      (NodeExpr)
import qualified Luna.Syntax.Graph.Node.Expr      as NodeExpr
import           Luna.Syntax.Graph.Node.MultiPart (MultiPartExpr, MultiPartSegment)
import qualified Luna.Syntax.Graph.Node.MultiPart as MultiPartExpr
import           Luna.Syntax.Label                (Label (Label))
import qualified Luna.Syntax.Label                as Label
import qualified Luna.Syntax.Lit                  as Lit
import           Luna.Syntax.Module               (LModule)
import qualified Luna.Syntax.Module               as Module
import qualified Luna.Syntax.Name.Pattern         as Pattern
import qualified Luna.Syntax.Native               as Native
import qualified Luna.Syntax.Pat                  as Pat
import           Luna.Syntax.Type                 (LType)
import qualified Luna.Syntax.Type                 as Type



replace :: (l1 -> l2) -> LModule l1 (LExpr l1 v) -> LModule l2 (LExpr l2 v)
replace conv = (Label.label %~ conv)
             . (Label.element . Module.body %~ map (replaceDecl conv))


replaceCons :: (l1 -> l2) -> Decl.LCons l1 (LExpr l1 v) -> Decl.LCons l2 (LExpr l2 v)
replaceCons conv (Label l1 (Decl.Cons name fields)) =
    Label (conv l1) $ Decl.Cons name $ map (replaceField conv) fields


replaceDataDecl :: (l1 -> l2) -> Decl.DataDecl l1 (LExpr l1 v) -> Decl.DataDecl l2 (LExpr l2 v)
replaceDataDecl conv (Decl.DataDecl name params cons decls) =
    Decl.DataDecl name params (map (replaceCons conv) cons)
                              (map (replaceDecl conv) decls)


replaceDecl :: (l1 -> l2) -> LDecl l1 (LExpr l1 v) -> LDecl l2 (LExpr l2 v)
replaceDecl conv (Label l1 decl) = Label (conv l1) $ case decl of
    Decl.Data    dataDecl        -> Decl.Data    (replaceDataDecl conv dataDecl)
    Decl.Func    funcDecl        -> Decl.Func    (replaceFuncDecl conv funcDecl)
    Decl.TpAls   dstType srcType -> Decl.TpAls   (replaceType conv dstType) (replaceType conv srcType)
    Decl.TpWrp   dstType srcType -> Decl.TpWrp   (replaceType conv dstType) (replaceType conv srcType)
    Decl.Foreign foreig          -> Decl.Foreign (replaceForeign conv foreig)
    Decl.Imp     imp             -> Decl.Imp     imp
    Decl.Pragma  pragma          -> Decl.Pragma  pragma


replaceField :: (l1 -> l2) -> Decl.LField l1 (LExpr l1 v) -> Decl.LField l2 (LExpr l2 v)
replaceField conv (Label l1 (Decl.Field ftype fname fval)) =
    Label (conv l1) $ Decl.Field (replaceType conv ftype) fname (fmap (replaceExpr conv) fval)


replaceFuncDecl :: (l1 -> l2) -> Decl.FuncDecl l1 (LExpr l1 v) [LExpr l1 v] -> Decl.FuncDecl l2 (LExpr l2 v) [LExpr l2 v]
replaceFuncDecl conv (Decl.FuncDecl path sig output body) =
    Decl.FuncDecl path (replaceArgPat conv sig) (replaceFuncOutput conv output)
                       (map (replaceExpr conv) body)


replaceFuncDecl' :: (l1 -> l2) -> Decl.FuncDecl l1 (LExpr l1 v) a -> Decl.FuncDecl l2 (LExpr l2 v) a
replaceFuncDecl' conv (Decl.FuncDecl path sig output body) =
    Decl.FuncDecl path (replaceArgPat conv sig) (replaceFuncOutput conv output) body


replaceLabel :: (l1 -> l2) -> Label l1 a -> Label l2 a
replaceLabel conv = Label.label %~ conv


replaceType :: (l1 -> l2) -> LType l1 -> LType l2
replaceType conv (Label l1 tpe) = Label (conv l1) $ case tpe of
    Type.Function inputs output -> Type.Function (map r inputs) (r output)
    Type.App      src    args   -> Type.App      (r src) (map r args)
    Type.Var      vname         -> Type.Var      vname
    Type.Tuple    items         -> Type.Tuple    (map r items)
    Type.List     item          -> Type.List     (r item)
    Type.Con      segments      -> Type.Con      segments
    Type.Meta     meta          -> Type.Meta     (replaceLabel conv meta)
    Type.Wildcard               -> Type.Wildcard
    where r = replaceType conv


replaceExpr :: (l1 -> l2) -> LExpr l1 v -> LExpr l2 v
replaceExpr conv (Label l1 expr) = Label (conv l1) $ case expr of
    Expr.Lambda      inputs output body -> Expr.Lambda (map (replaceExprArg conv) inputs) (fmap (replaceType conv) output) (map (replaceExpr conv) body)
    Expr.RecUpd      vname fieldUpds    -> Expr.RecUpd vname (map (replaceFieldUpd conv) fieldUpds)
    Expr.Case        expr match         -> Expr.Case  (replaceExpr conv expr) (map (replaceMatch conv) match)
    Expr.Typed       cls expr           -> Expr.Typed (replaceType conv cls) (replaceExpr conv expr)
    Expr.Assignment  dst src            -> Expr.Assignment (replaceLPat conv dst) (replaceExpr conv src)
    Expr.Accessor    acc src            -> Expr.Accessor acc (replaceExpr conv src)
    Expr.Curry       expr               -> Expr.Curry (replaceExpr conv expr)
    Expr.Meta        meta               -> Expr.Meta (replaceLabel conv meta)
    Expr.Tuple       items              -> Expr.Tuple (map (replaceExpr conv) items)
    Expr.Grouped     expr               -> Expr.Grouped (replaceExpr conv expr)
    Expr.Cons        cname              -> Expr.Cons cname
    Expr.Decl        decl               -> Expr.Decl (replaceDecl conv decl)
    Expr.Lit         lit                -> Expr.Lit (replaceLit conv lit)
    Expr.Native      native             -> Expr.Native (replaceNative conv native)
    Expr.Var         ident              -> Expr.Var ident
    Expr.List        list               -> Expr.List (replaceList conv list)
    Expr.App         exprApp            -> Expr.App (replaceNamePat conv exprApp)
    Expr.Wildcard                       -> Expr.Wildcard


replaceForeign :: (l1 -> l2) -> Foreign (Decl.ForeignDecl l1 (LExpr l1 v)) -> Foreign (Decl.ForeignDecl l2 (LExpr l2 v))
replaceForeign conv (Foreign target a) = Foreign target $ replaceForeignDecl conv a


replaceForeignDecl :: (l1 -> l2) -> Decl.ForeignDecl l1 (LExpr l1 v) -> Decl.ForeignDecl l2 (LExpr l2 v)
replaceForeignDecl conv foreignDecl = case foreignDecl of
    Decl.FData dataDecl -> Decl.FData (replaceDataDecl  conv dataDecl)
    Decl.FFunc funcDecl -> Decl.FFunc (replaceFuncDecl' conv funcDecl)
    Decl.FImp  text     -> Decl.FImp  text


replaceArgPat :: (l1 -> l2) -> Pattern.NamePat Pattern.SegmentName (Arg l1 (LExpr l1 v))
                            -> Pattern.NamePat Pattern.SegmentName (Arg l2 (LExpr l2 v))
replaceArgPat conv (Pattern.NamePat prefix base segmentList) =
    Pattern.NamePat (fmap (replaceArg conv) prefix)
                    (replaceSegment conv base)
                    (map (replaceSegment conv) segmentList)


replaceSegment :: (l1 -> l2) -> Pattern.Segment Pattern.SegmentName (Arg l1 (LExpr l1 v)) -> Pattern.Segment Pattern.SegmentName (Arg l2 (LExpr l2 v))
replaceSegment conv (Pattern.Segment base args) = Pattern.Segment base (fmap (replaceArg conv) args)


replaceArg :: (l1 -> l2) -> (Arg l1 (LExpr l1 v)) -> (Arg l2 (LExpr l2 v))
replaceArg conv (A.Arg pat val) = A.Arg (conv' pat) (fmap (replaceExpr conv) val )
        where conv' (Label l1 pat) = Label (conv l1) (replacePat conv pat)


replacePat :: (l1 -> l2) -> Pat.Pat l1 -> Pat.Pat l2
replacePat conv a = case a of
    Pat.App src args     -> Pat.App   (replaceLPat conv src) (fmap (replaceLPat conv) args)
    Pat.Typed pat typ    -> Pat.Typed (replaceLPat conv pat) (replaceType conv typ)
    Pat.Grouped pat      -> Pat.Grouped (replaceLPat conv pat)
    Pat.Lit (Label l1 a) -> Pat.Lit (Label (conv l1) a)
    Pat.Tuple items      -> Pat.Tuple (fmap (replaceLPat conv) items)
    Pat.Con cname        -> Pat.Con cname
    Pat.Var vname        -> Pat.Var vname
    Pat.Wildcard         -> Pat.Wildcard
    Pat.RecWildcard      -> Pat.RecWildcard


replaceLPat :: (l1 -> l2) -> Label l1 (Pat.Pat l1) -> Label l2 (Pat.Pat l2)
replaceLPat conv (Label l1 pat) = Label (conv l1) (replacePat conv pat)


replaceLit :: (l1 -> l2) -> Lit.LLit l1 -> Lit.LLit l2
replaceLit conv (Label l1 lit) = Label (conv l1) lit


replaceList :: (l1 -> l2) -> (Expr.List (LExpr l1 v)) -> (Expr.List (LExpr l2 v))
replaceList conv list = case list of
    Expr.SeqList es    -> Expr.SeqList   (map (replaceExpr conv) es)
    Expr.RangeList seq -> Expr.RangeList (replaceSequence  conv seq)


replaceSequence :: (l1 -> l2) -> Expr.Sequence (LExpr l1 v) -> Expr.Sequence (LExpr l2 v)
replaceSequence conv seq = case seq of
                        Expr.Linear a b -> Expr.Linear (replaceExpr conv a) (fmap (replaceExpr conv) b)
                        Expr.Geometric a a1 b -> Expr.Geometric (replaceExpr conv a) (replaceExpr conv a1) (fmap (replaceExpr conv) b)


replaceNative :: (l1 -> l2) -> Native.Native (LExpr l1 v) -> Native.Native (LExpr l2 v)
replaceNative conv native = case native of
    Native.AST a  -> Native.AST $ replaceExpr conv a
    Native.Code segments -> Native.Code segments


replaceNamePat :: (l1 -> l2) -> Pattern.NamePat (LExpr l1 v) (Expr.AppArg (LExpr l1 v)) ->
                                Pattern.NamePat (LExpr l2 v) (Expr.AppArg (LExpr l2 v))
replaceNamePat conv (Pattern.NamePat pref b sl) = Pattern.NamePat newPref newB newSl
    where newPref = fmap (replaceAppArg conv) pref
          newB    = Pattern.Segment (replaceExpr conv (Pattern._segmentBase b)) (map (replaceAppArg conv) (Pattern._segmentArgs b))    -- Segment (LExpr l1 v) (Expr.AppArg (LExpr l1 v))
          newSl   = map (replaceSegmentApp conv) sl


replaceAppArg :: (l1 -> l2) -> Expr.AppArg (LExpr l1 v) -> Expr.AppArg (LExpr l2 v)
replaceAppArg conv (Expr.AppArg margn expr) = Expr.AppArg margn (replaceExpr conv expr)


replaceSegmentApp :: (l1 -> l2) -> Pattern.Segment Pattern.SegmentName (Expr.AppArg (LExpr l1 v)) -> Pattern.Segment Pattern.SegmentName (Expr.AppArg (LExpr l2 v))
replaceSegmentApp conv (Pattern.Segment base args) = Pattern.Segment base (fmap (replaceAppArg conv) args)


replaceFuncOutput :: (l1 -> l2) -> Maybe (Type.LType l1) -> Maybe (Type.LType l2)
replaceFuncOutput conv funcout = fmap (replaceType conv) funcout


replaceFieldUpd :: (l1 -> l2) -> Expr.FieldUpd l1 v -> Expr.FieldUpd l2 v
replaceFieldUpd conv (Expr.FieldUpd sel lexpr) = Expr.FieldUpd sel (replaceExpr conv lexpr)


replaceExprArg :: (l1 -> l2) -> Expr.ExprArg l1 v -> Expr.ExprArg l2 v
replaceExprArg conv (Label l1 arg) = Label (conv l1) (replaceArg conv arg)


replaceMatch :: (l1 -> l2) -> Expr.LMatch l1 v -> Expr.LMatch l2 v
replaceMatch conv (Label l1 (Expr.Match lpat mbody)) = Label (conv l1) (Expr.Match newPat newMBody)
    where newPat   = replaceLPat conv lpat
          newMBody = map (replaceExpr conv) mbody


replaceGraph :: (l1 -> l2) -> Graph l1 v -> Graph l2 v
replaceGraph conv = Graph.nmap (replaceNode conv)


replaceNode :: (l1 -> l2) -> Node l1 v -> Node l2 v
replaceNode conv node = case node of
    Node.Expr expr outputPat defaults pos groupInfo generated ->
        Node.Expr (replaceNodeExpr conv expr)
                  (replaceLPat conv <$> outputPat)
                  (replaceDefaultsMap conv defaults)
                  pos (map conv groupInfo) generated
    Node.Inputs pos -> Node.Inputs pos
    Node.Outputs defaults pos -> Node.Outputs (replaceDefaultsMap conv defaults) pos


replaceDefaultsMap :: (l1 -> l2) -> DefaultsMap l1 v -> DefaultsMap l2 v
replaceDefaultsMap conv = DefaultsMap.map (replaceExpr conv)


replaceNodeExpr :: (l1 -> l2) -> NodeExpr l1 v -> NodeExpr l2 v
replaceNodeExpr conv nodeExpr = case nodeExpr of
    NodeExpr.StringExpr strExpr -> NodeExpr.StringExpr strExpr
    NodeExpr.MultiPart mp -> NodeExpr.MultiPart $ replaceMultiPart conv mp
    NodeExpr.ASTExpr expr -> NodeExpr.ASTExpr $ replaceExpr conv expr


replaceMultiPart :: (l1 -> l2) -> MultiPartExpr l1 -> MultiPartExpr l2
replaceMultiPart conv = MultiPartExpr.base %~ replaceMultiPartSegment conv


replaceMultiPartSegment :: (l1 -> l2) -> MultiPartSegment (l1, a) -> MultiPartSegment (l2, a)
replaceMultiPartSegment conv = MultiPartExpr.segmentBase . _1 %~ conv

