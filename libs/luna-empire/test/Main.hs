{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prologue
import qualified Luna.Syntax.Builder.Star             as StarBuilder
import qualified Luna.Syntax.Builder.Node             as NodeBuilder
import qualified Luna.Syntax.Builder                  as Builder
import           Luna.Syntax.Builder
import           Luna.Syntax.Repr.Graph
import           Luna.Diagnostic.AST (toGraphViz, render)
import           Empire.Data.AST
import           Empire.Commands.Project
import           Empire.Empire
import           Empire.Commands.Project
import           Empire.Commands.Library


sampleGraph :: ((), AST)
sampleGraph = runIdentity
              $ flip StarBuilder.evalT Nothing
              $ flip Builder.runT def
              $ flip NodeBuilder.evalT (Ref $ Node (0 :: Int))
              $ do
                  dsraka   <- _string "dupasraka"
                  i1       <- _int 433
                  i2       <- _int 421
                  i3       <- _int 444
                  b        <- _blank
                  _string "aa"
                  _string "bb"
                  namePlus <- _string "+"
                  dupaSrakaMethod <- accessor dsraka b
                  accPlus  <- accessor namePlus i1
                  appPlus  <- app accPlus [arg i2, arg i3]
                  {-app (accessor (_string "+") (_int 2)) [arg $ _int 3]-}
                  return ()

test = do
    (id, _) <- createProject (Just "dupa") "/no/elo"
    createLibrary id (Just "xd") "/xd/xd"

main = do
    (_, s) <- runEmpire def test
    print s
{-main = return ()-}
