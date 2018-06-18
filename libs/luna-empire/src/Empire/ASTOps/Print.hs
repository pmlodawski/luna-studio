{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

module Empire.ASTOps.Print where

import           Control.Lens                   (non)
import           Control.Monad                  ((<=<))
import           Data.List                      (delete)
import qualified Data.Map                       as Map
import qualified Data.Text                      as Text
import           Empire.Prelude

import           Empire.ASTOp                   (ASTOpReq, GraphOp, match)
import qualified Empire.ASTOps.Read             as ASTRead
import           Empire.Data.AST                (EdgeRef, NodeRef)
import           Empire.Data.Graph              (Graph)
import qualified Luna.IR                        as IR
-- import           Luna.IR.Term.Uni
import           LunaStudio.Data.TypeRep

import           Luna.Syntax.Text.Lexer.Grammar (isOperator)
import qualified Luna.Syntax.Prettyprint as Prettyprint
-- import           Luna.Syntax.Text.Pretty.Pretty as CodeGen

getTypeRep :: GraphOp m => NodeRef -> m TypeRep
getTypeRep tp = match tp $ \case
    -- Monadic s _   -> getTypeRep =<< source s
    Cons   n args -> TCons (nameToString n) <$> (mapM (getTypeRep <=< source) =<< ptrListToList args)
    Lam    a out  -> TLam <$> (getTypeRep =<< source a) <*> (getTypeRep =<< source out)
    Acc    t n    -> TAcc (nameToString n) <$> (getTypeRep =<< source t)
    Var    n      -> return $ TVar $ delete '#' $ nameToString n
    IRNumber{}    -> return $ TCons "Number" []
    _             -> return TStar

-- instance ASTOpReq Graph m => Compactible t CompactStyle m where
--     shouldBeCompact _ r = ASTRead.isGraphNode r

printExpression :: GraphOp m => NodeRef -> m String
printExpression = const $ return "exp" -- fmap convert . CodeGen.subpass CompactStyle . generalize

printFullExpression :: GraphOp m => NodeRef -> m Text
printFullExpression n = match n $ \case
    Marked m e -> return "markedExpr!!!!!!"
    _          -> Prettyprint.run @Prettyprint.Simple def n

printName :: GraphOp m => NodeRef -> m String
printName node = convert <$> Prettyprint.run @Prettyprint.Simple def node

printNodeTarget :: GraphOp m => NodeRef -> m String
printNodeTarget ref = match ref $ \case
    Unify _ r -> printExpression =<< source r
    _         -> printExpression ref

genOperatorName :: IR.Name -> Text
genOperatorName op = operatorNamesMap ^. at op . non "operator" where
    operatorNamesMap = Map.fromList [ ("+",        "sum")
                                    , ("*",        "product")
                                    , ("-",        "difference")
                                    , ("/",        "quotient")
                                    , ("#uminus#", "negation")
                                    ]

genNodeBaseName :: GraphOp m => NodeRef -> m Text
genNodeBaseName ref = match ref $ \case
    App f a           -> recurOn $ generalize f
    Grouped g         -> recurOn $ generalize g
    -- LeftSection  op _ -> recurOn $ generalize op
    -- RightSection op _ -> recurOn $ generalize op
    Marked _ a        -> recurOn $ generalize a
    Lam{}             -> return "lambda"
    IRString{}        -> return "text"
    IRNumber{}        -> return "number"
    Tuple{}           -> return "tuple"
    List{}            -> return "list"
    Cons n _          -> return $ Text.toLower $ nameToText n
    Var n             -> return $ genOp n
    Acc t n           -> return $ genOp n
    _                 -> return $ "expr"
    where recurOn :: GraphOp m => EdgeRef -> m Text
          recurOn a = genNodeBaseName =<< source a
          genOp   n = if isOperator n  || n == "#uminus#" then genOperatorName n else nameToText n
