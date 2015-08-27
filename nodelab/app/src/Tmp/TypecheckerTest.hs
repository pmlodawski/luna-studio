{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoOverloadedStrings #-}


module Tmp.TypecheckerTest where

import           Utils.PreludePlus

import           Data.GraphViz.Types.Canonical
import           Data.GraphViz.Types
import           Data.GraphViz.Attributes.Complete   hiding (Label, Int)
import qualified Data.GraphViz.Attributes.Complete   as GV
import qualified Data.GraphViz.Attributes            as GV
import           Data.GraphViz.Printing              (toDot)
import           Data.GraphViz.Commands
import qualified Data.GraphViz.Attributes.Colors     as GVC
import qualified Data.GraphViz.Attributes.Colors.X11 as GVC
import           Data.GraphViz.Printing
import           Luna.Repr.Styles (HeaderOnly(..), Simple(..))

import           Data.Repr
import           Control.Monad.State

import           Luna.Syntax.Builder.Graph hiding (get, put)
import           Luna.Syntax.Builder

import           Luna.Syntax.Layer.Typed
import           Luna.Syntax.Layer.Labeled
import           Luna.Syntax.AST.Term
import           Luna.Syntax.AST.Decl
import qualified Luna.Diagnostic.AST as Diag

import qualified Data.Text.Lazy as Text

import           AST.AST

import           Utils.Viz



varA :: StateGraphMeta -> RefFunctionGraphMeta
varA bldrState = flip runGraphState bldrState $
    withMeta (Meta 1 "a") $ var "a"

varB :: StateGraphMeta -> RefFunctionGraphMeta
varB bldrState = flip runGraphState bldrState $
    withMeta (Meta 2 "b") $ var "b"

varF :: StateGraphMeta -> RefFunctionGraphMeta
varF bldrState = flip runGraphState bldrState $
    withMeta (Meta 1 "f") $ var "f"

accA :: GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
accA rv1 bldrState = flip runGraphState bldrState $
    withMeta (Meta 3 "c") $ rv1 @. "foo"

appA :: GraphRefMeta -> GraphRefMeta -> GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
appA rf rv1 rv2 bldrState = flip runGraphState bldrState $
    withMeta (Meta 4 "app1") $ rf @$ [arg rv1, arg rv2]

appB :: GraphRefMeta -> GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
appB rf rv1 bldrState = flip runGraphState bldrState $
    withMeta (Meta 4 "app2") $ rf @$ [arg rv1]


-- TODO: map id -> ref (GraphRefMeta)


rebuild :: g -> BldrState g
rebuild f = BldrState [] $ f

main :: IO ()
main = do
    let (rv1, a) = varA def
        (rv2, b) = varB $ rebuild a
        (rf1, c) = accA rv1 $ rebuild b
        (rv3, d) = appA rf1 rv1 rv2 $ rebuild c
        (rf2, e) = varF $ rebuild d
        (rv5, f) = appB rf2 rv3 $ rebuild e
        out      = c
    putStrLn "Typeckecker test:"
    print $ repr out

    let gv = Diag.toGraphViz $ out
    displayGraph $ printIt gv

    return ()
