module Main where


import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree

import Data.Graph.Inductive.Monad
-- import Data.Graph.Inductive.Monad.IOArray


-- | generate list of unlabeled nodes
genUNodes :: Int -> [UNode]
genUNodes n = zip [1..n] (repeat ())

-- | generate list of labeled nodes
genLNodes :: Enum a => a -> Int -> [LNode a]
genLNodes q i = take i (zip [1..] [q..])

-- | denote unlabeled edges
labUEdges :: [Edge] -> [UEdge]
labUEdges = map (\(i,j) -> (i,j,()))

-- | empty (unlabeled) edge list
noEdges :: [UEdge]
noEdges = []


a,b,c,e,loop,ab,abb,dag3   :: Gr Char ()
e3                         :: Gr () String
cyc3,g3,g3b                :: Gr Char String
dag4                       :: Gr Int ()
d1,d3                      :: Gr Int Int

a    = ([],1,'a',[]) & empty                  -- just a node

b    = mkGraph (zip [1..2] "ab") noEdges      -- just two nodes

c    = mkGraph (zip [1..3] "abc") noEdges     -- just three nodes

e    = ([((),1)],2,'b',[]) & a                -- just one edge a-->b

e3   = mkGraph (genUNodes 2)
       [(1,2,"a"),(1,2,"b"),(1,2,"a")]        -- three edges (two labels) a-->b

loop = ([],1,'a',[((),1)]) & empty            -- loop on single node

ab   = ([((),1)],2,'b',[((),1)]) & a          -- cycle of two nodes:  a<-->b

abb  = mkGraph (zip [1..2] "ab") (labUEdges [(2,2)]) -- a and loop on b

cyc3 = buildGr                                -- cycle of three nodes
       [([("ca",3)],1,'a',[("ab",2)]),
                ([],2,'b',[("bc",3)]),
                ([],3,'c',[])]

dag3 = mkGraph (zip [1..3] "abc") (labUEdges [(1,3)])
dag4 = mkGraph (genLNodes 1 4) (labUEdges [(1,2),(1,4),(2,3),(2,4),(4,3)])

d1   = mkGraph (genLNodes 1 2) [(1,2,1)]
d3   = mkGraph (genLNodes 1 3) [(1,2,1),(1,3,4),(2,3,2)]

g3 = ([("left",2),("up",3)],1,'a',[("right",2)]) & (
                        ([],2,'b',[("down",3)])  & (
                        ([],3,'c',[])            & empty ))
g3b = ([("down",2)], 3,'c',[("up",1)])   & (
      ([("right",1)],2,'b',[("left",1)]) & (
                 ([],1,'a',[])           & empty ))
