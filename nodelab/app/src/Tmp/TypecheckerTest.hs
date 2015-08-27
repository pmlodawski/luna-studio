{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}


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

import           Typechecker.Typechecker

import           Tmp.Viz


test1 :: FunctionGraphMeta
test1 = runIdentity $ flip evalStateT def $ flip execFunctionBuilderT def $ do
    a <- withMeta (Meta 7 "g") $ var "a"
    put $ Meta 1 "a"
    b <- var "b"
    put $ Meta 2 "b"
    x <- var "x" @. "foo"
    put $ Meta 3 "c"
    y <- x @$ [arg a]
    return ()

test2 :: FunctionGraphMeta
test2 = runIdentity $ flip evalStateT def $ flip execFunctionBuilderT def $ do
    a <- withMeta (Meta 1 "a") $ var "a"
    b <- withMeta (Meta 2 "b") $ var "b"
    x <- withMeta (Meta 3 "c") $ var "x" @. "foo"
    y <- withMeta (Meta 4 "d") $ x @$ [arg a]
    return ()


nytst2f :: RefFunctionGraphMeta
nytst2f = flip runFunctionBuilderState def $ do
    v1 <- var "foo"
    v2 <- var "bar"
    s  <- star
    a  <- v1 @. "x"
    x  <- v1 @$ [arg v2]
    y  <- x @. "y"
    return v1


varA :: StateGraphMeta -> RefFunctionGraphMeta
varA bldrState = flip runFunctionBuilderState bldrState $
    withMeta (Meta 1 "a") $ var "a"

varB :: StateGraphMeta -> RefFunctionGraphMeta
varB bldrState = flip runFunctionBuilderState bldrState $
    withMeta (Meta 2 "b") $ var "b"

varF :: StateGraphMeta -> RefFunctionGraphMeta
varF bldrState = flip runFunctionBuilderState bldrState $
    withMeta (Meta 1 "f") $ var "f"

accA :: GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
accA rv1 bldrState = flip runFunctionBuilderState bldrState $
    withMeta (Meta 3 "c") $ rv1 @. "foo"

appA :: GraphRefMeta -> GraphRefMeta -> GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
appA rf rv1 rv2 bldrState = flip runFunctionBuilderState bldrState $
    withMeta (Meta 4 "app1") $ rf @$ [arg rv1, arg rv2]

appB :: GraphRefMeta -> GraphRefMeta -> StateGraphMeta -> RefFunctionGraphMeta
appB rf rv1 bldrState = flip runFunctionBuilderState bldrState $
    withMeta (Meta 4 "app2") $ rf @$ [arg rv1]

-- funB :: GraphRefMeta -> _ -> StateGraphMeta -> RefFunctionGraphMeta
-- funB rf args bldrState = flip runFunctionBuilderState bldrState $
--     withMeta (Meta 4 "d") $ rf @$ args
-- -- example: (rv4, e) = funB rf1 [arg rv1, arg rv2] $ rebuild d


-- TODO: map id -> ref (GraphRefMeta)


rebuild :: Function g -> BldrState g
rebuild f = BldrState [] $ f ^. body

main :: IO ()
main = do
    let (rv1, a) = varA def
        (rv2, b) = varB $ rebuild a
        (rf1, c) = accA rv1 $ rebuild b
        (rv3, d) = appA rf1 rv1 rv2 $ rebuild c
        (rf2, e) = varF $ rebuild d
        (rv5, f) = appB rf2 rv3 $ rebuild e
        out      = f
    putStrLn "Typeckecker test:"
    print $ repr out

    let gv = Diag.toGraphViz $ out ^. body
    displayGraph $ printIt gv

    return ()
