---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}

module Luna.Pass.Transform.Graph.Parser.Parser where

--import           Control.Monad.State
--import           Control.Monad.Trans.Either
--import qualified Data.IntSet                as IntSet
--import qualified Data.List                  as List
--import qualified Data.Text.Lazy             as Text

--import           Flowbox.Prelude                        hiding (error, folded, mapM, mapM_)
--import           Flowbox.System.Log.Logger
--import qualified Luna.Parser.Token                      as Token
--import qualified Luna.Pass.Analysis.ID.ExtractIDs       as ExtractIDs
--import qualified Luna.Pass.Pass                         as Pass
--import qualified Luna.Pass.Transform.AST.IDFixer.State  as IDFixer
--import           Luna.Pass.Transform.Graph.Parser.State (GPPass)
--import qualified Luna.Pass.Transform.Graph.Parser.State as State
--import           Luna.Syntax.Arg                        (Arg (Arg))
--import qualified Luna.Syntax.Decl                       as Decl
--import qualified Luna.Syntax.Enum                       as Enum
--import           Luna.Syntax.Expr                       (LExpr)
--import qualified Luna.Syntax.Expr                       as Expr
--import           Luna.Syntax.Graph.Graph                (Graph)
--import qualified Luna.Syntax.Graph.Graph                as Graph
--import           Luna.Syntax.Graph.Node                 (Node)
--import qualified Luna.Syntax.Graph.Node                 as Node
--import           Luna.Syntax.Graph.Node.Expr            (NodeExpr)
--import qualified Luna.Syntax.Graph.Node.Expr            as NodeExpr
--import qualified Luna.Syntax.Graph.Node.StringExpr      as StringExpr
--import qualified Luna.Syntax.Graph.Port                 as Port
--import           Luna.Syntax.Label                      (Label (Label))
--import qualified Luna.Syntax.Label                      as Label
--import qualified Luna.Syntax.Name                       as Name
--import qualified Luna.Syntax.Name.Pattern               as Pattern
--import qualified Luna.Syntax.Native                     as Native
--import           Luna.Syntax.Pat                        (LPat)
--import qualified Luna.Syntax.Pat                        as Pat
--import           Luna.System.Pragma.Store               (MonadPragmaStore)


----FIXME[PM]: remove
--patternParser :: String -> Int -> Either String (LPat a, Int)
--patternParser = undefined
--exprParser :: String -> Int -> Either String (LExpr a v, Int)
--exprParser = undefined
-------------------


--type FuncDecl a e v = Decl.FuncDecl a e [Expr.LExpr a v]
--type V = ()


--logger :: Logger
--logger = getLogger $(moduleName)


--run :: (Eq a, Enum.Enumerated a, MonadPragmaStore m)
--    => Graph a V -> PropertyMap a V -> FuncDecl a e V
--    -> EitherT Pass.PassError m (FuncDecl a e V, PropertyMap a V)
--run gr pm = Pass.run_ (Pass.Info "GraphParser") (State.make gr pm) . graph2expr


--graph2expr :: Eq a => FuncDecl a e V -> GPPass a V m (FuncDecl a e V, PropertyMap a V)
--graph2expr funcDecl = do
--    let sig = funcDecl ^. Decl.funcDeclSig
--    graph <- State.getGraph
--    mapM_ (parseNode sig) $ Graph.sort graph
--    b  <- State.getBody
--    mo <- State.getOutput
--    let body = reverse $ case mo of
--                Nothing -> b
--                Just o  -> o : b
--    pm <- State.getPropertyMap
--    return (funcDecl & Decl.funcDeclBody .~ body, pm)


--label :: Enum.Enumerated l => Enum.ID -> a -> Label l a
--label num = Label (Enum.tag num)


--labelUnknown :: Enum.Enumerated l => a -> Label l a
--labelUnknown = Label (Enum.tag IDFixer.unknownID)


--parseNode :: Decl.FuncSig a e -> (Node.ID, Node a V) -> GPPass a V m ()
--parseNode signature (nodeID, node) = do
--    case node of
--        Node.Expr    expr _ _ -> parseExprNode    nodeID expr
--        Node.Inputs  {}       -> parseInputsNode  nodeID signature
--        Node.Outputs {}       -> parseOutputsNode nodeID
--    State.setPosition    nodeID $ node ^. Node.pos


----TODO[PM] : zrobić z tego typeclass
--parseExprNode :: Node.ID -> NodeExpr a V -> GPPass a V m ()
--parseExprNode nodeID nodeExpr = case nodeExpr of
--    NodeExpr.StringExpr strExpr -> case strExpr of
--        StringExpr.List           -> parseListNode    nodeID
--        StringExpr.Tuple          -> parseTupleNode   nodeID
--        StringExpr.Pattern pat    -> parsePatNode     nodeID pat
--        StringExpr.Native  native -> parseNativeNode  nodeID native
--        _                         -> parseAppNode     nodeID $ StringExpr.toString strExpr
--    NodeExpr.ASTExpr expr   -> parseASTExprNode nodeID expr


--parseInputsNode :: Node.ID -> Decl.FuncSig a e -> GPPass a V m ()
--parseInputsNode nodeID = mapM_ (parseArg nodeID) . zip [0..] . Pattern.args


--parseArg :: Node.ID -> (Int, Arg a e) -> GPPass a V m ()
--parseArg nodeID (num, input) = case input of
--    Arg (Label _                     (Pat.Var vname)    ) _ -> addVar vname
--    Arg (Label _ (Pat.Typed (Label _ (Pat.Var vname)) _)) _ -> addVar vname
--    _                                                       -> lift $ left "parseArg: Wrong Arg type"
--    where addVar vname = State.addToNodeMap (nodeID, Port.Num num) $ labelUnknown $ Expr.Var $ Expr.Variable vname ()


--parseOutputsNode :: Node.ID -> GPPass a V m ()
--parseOutputsNode nodeID = do
--    srcs    <- State.getNodeSrcs nodeID
--    inPorts <- State.inboundPorts nodeID
--    case (srcs, inPorts) of
--        ([], _)               -> whenM State.doesLastStatementReturn $
--                                   State.setOutput $ labelUnknown $ Expr.Grouped
--                                                   $ labelUnknown $ Expr.Tuple []
--        ([src], [Port.Num 0]) -> State.setOutput $ labelUnknown $ Expr.Grouped src
--        ([src], _           ) -> State.setOutput src
--        _                     -> State.setOutput $ labelUnknown $ Expr.Tuple srcs


--patVariables :: LPat a -> [LExpr a V]
--patVariables pat = case pat of
--    Label l (Pat.Var   name  ) -> [Label l $ Expr.Var $ Expr.Variable name ()]
--    Label _ (Pat.Tuple items ) -> concatMap patVariables items
--    Label _ (Pat.App   _ args) -> concatMap patVariables args
--    _                          -> []


----patchedParserState :: ASTInfo
----                   -> ParserState.State (Pragma Pragma.ImplicitSelf, (Pragma Pragma.AllowOrphans, (Pragma Pragma.TabLength, ())))
----patchedParserState info' = def
----    & ParserState.info .~ info'
----    & ParserState.conf .~ parserConf
----    where parserConf  = Parser.defConfig & Config.setPragma Pragma.AllowOrphans


--parsePatNode :: Node.ID -> String -> GPPass a V m ()
--parsePatNode nodeID pat = do
--    srcs <- State.getNodeSrcs nodeID
--    case srcs of
--        [s] -> do
--            p <- case patternParser pat IDFixer.unknownID of
--                    Left  er     -> lift $ left $ show er
--                    Right (p, _) -> return p
--            let e = label nodeID $ Expr.Assignment p s
--            case patVariables p of
--                [var] -> State.addToNodeMap (nodeID, Port.All) var
--                vars  -> mapM_ (\(i, v) -> State.addToNodeMap (nodeID, Port.Num i) v) $ zip [0..] vars
--            State.addToBody e
--        _      -> lift $ left "parsePatNode: Wrong Pat arguments"


--parseNativeNode :: Node.ID -> String -> GPPass a V m ()
--parseNativeNode nodeID native = do
--    srcs <- State.getNodeSrcs nodeID
--    expr <- case exprParser native nodeID of
--                    Left  er     -> lift $ left $ show er
--                    Right (e, _) -> return e
--    addExpr nodeID $ replaceNativeVars srcs expr


--replaceNativeVars :: [LExpr a v] -> LExpr a v -> LExpr a v
--replaceNativeVars srcs native = native & Label.element . Expr.native . Native.segments %~ replaceNativeVars' srcs where
--    replaceNativeVars' []                                             segments              = segments
--    replaceNativeVars' _                                              []                    = []
--    replaceNativeVars' vars                                           (sh@Native.Str {}:st) =  sh                                 : replaceNativeVars' vars st
--    replaceNativeVars' (Label _ (Expr.Var (Expr.Variable name _)):vt) (sh@Native.Var {}:st) = (sh & Native.name .~ toString name) : replaceNativeVars' vt   st


--parseAppNode :: Node.ID -> String -> GPPass a V m ()
--parseAppNode nodeID app = do
--    srcs <- State.getNodeSrcs nodeID
--    expr <- if isOperator app
--                then return $ label nodeID $ Expr.Var $ Expr.Variable (fromString app) ()
--                else do
--                    case exprParser app nodeID of
--                        Left  er     -> lift $ left $ show er
--                        Right (e, _) -> return e
--    let ids = ExtractIDs.run expr
--    mapM_ State.setGraphFolded $ IntSet.toList $ IntSet.delete nodeID ids
--    State.setGraphFoldTop nodeID $ exprToNodeID expr
--    let requiresApp (Expr.Cons {}) = True
--        requiresApp _              = False
--    case srcs of
--        []                      -> addExpr nodeID $ if requiresApp $ unwrap expr
--                                       then labelUnknown $ Expr.app expr []
--                                       else expr
--        Label _ Expr.Wildcard:t -> addExpr nodeID $ labelUnknown $ Expr.app expr $ map Expr.unnamed t
--        f:t                     -> addExpr nodeID $ labelUnknown $ Expr.app acc  $ map Expr.unnamed t
--                                        where acc = label nodeID $ Expr.Accessor (Name.mkNameBaseAccessor $ Text.pack app) f


--exprToNodeID :: Enum.Enumerated a => LExpr a v -> Node.ID
--exprToNodeID (Label _ (Expr.App exprApp)) = exprToNodeID $ exprApp ^. Pattern.base . Pattern.segmentBase
--exprToNodeID lexpr                        = Enum.id $ lexpr ^. Label.label


--setNodeID :: Enum.Enumerated a => Node.ID -> LExpr a v -> LExpr a v
--setNodeID nodeID lexpr@(Label _ (Expr.App _)) = lexpr & Label.element . Expr.exprApp
--                                                      . Pattern.base . Pattern.segmentBase
--                                                      %~ setNodeID nodeID
--setNodeID nodeID lexpr                        = lexpr & Label.label              .~ Enum.tag  nodeID


--parseTupleNode :: Node.ID -> GPPass a V m ()
--parseTupleNode nodeID = do
--    srcs <- State.getNodeSrcs nodeID
--    let e = label nodeID $ Expr.Tuple srcs
--    addExpr nodeID e


----parseGroupedNode :: Node.ID -> GPPass ()
----parseGroupedNode nodeID = do
----    [src] <- State.getNodeSrcs nodeID
----    let e = Expr.Grouped nodeID src
----    addExpr nodeID e


--parseListNode :: Node.ID -> GPPass a V m ()
--parseListNode nodeID = do
--    srcs <- State.getNodeSrcs nodeID
--    let e = label nodeID $ Expr.List $ Expr.SeqList srcs
--    addExpr nodeID e


--parseASTExprNode :: Node.ID -> LExpr a V -> GPPass a V m ()
--parseASTExprNode nodeID = addExpr nodeID
--                        . setNodeID nodeID


--addExpr :: Node.ID -> LExpr a V  -> GPPass a V m ()
--addExpr nodeID expr' = do
--    graph <- State.getGraph

--    flags <- State.getFlags nodeID
--    let folded         = Flags.isSet' flags $ view Flags.astFolded
--        assignment     = Flags.isSet' flags $ view Flags.astAssignment
--        defaultNodeGen = Flags.isSet' flags $ view Flags.defaultNodeGenerated
--        grouped        = Flags.isSet' flags $ view Flags.grouped

--    let assignmentEdge (dstID, dst, _) = (not $ Node.isOutputs dst) || (length (Graph.lprelData graph dstID) > 1)
--        assignmentCount = length $ List.filter assignmentEdge
--                                 $ Graph.lsuclData graph nodeID

--        connectedToOutput = List.any (Node.isOutputs . view _2)
--                          $ Graph.lsuclData graph nodeID

--        expr = if grouped
--            then labelUnknown $ Expr.Grouped expr'
--            else expr'

--    if (folded && assignmentCount == 1) || defaultNodeGen
--        then State.addToNodeMap (nodeID, Port.All) expr
--        else if assignment || assignmentCount > 1
--            then do outName <- State.getNodeOutputName nodeID
--                    let p = labelUnknown $ Pat.Var  outName
--                        v = labelUnknown $ Expr.Var $ Expr.Variable outName ()
--                        a = labelUnknown $ Expr.Assignment p expr
--                    State.addToNodeMap (nodeID, Port.All) v
--                    State.addToBody a
--            else do State.addToNodeMap (nodeID, Port.All) expr
--                    unless (connectedToOutput || assignmentCount == 1) $
--                        State.addToBody expr


--isOperator :: String -> Bool
--isOperator expr = length expr == 1 && head expr `elem` Token.opChars
