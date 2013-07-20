---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Network.Def.NodeDef(
NodeDef(..),
ID,
empty,
noImports,
noPorts
) where

import           Luna.Type.Type             (Type)
import qualified Luna.Lib.Library         as Library
import           Luna.Network.Graph.Graph   (Graph)
import qualified Luna.Network.Graph.Graph as Graph

data NodeDef = NotLoaded
             | NodeDef {
                 cls     :: Type,
                 inputs  :: [String],
                 outputs :: [String],
                 imports :: [String],
                 graph   :: Graph,
                 libID   :: Library.ID
               } deriving (Show)

type ID   = Int

--FIXME[wd]: Czy libID powinno być ustawiane na 0?
empty :: Type -> NodeDef
empty t = NodeDef t noPorts noPorts noImports Graph.empty 0

noImports :: [String]
noImports = []

noPorts :: [String]
noPorts = []

------------------------- INSTANCES -------------------------

--instance Serialize NodeDef where
--  put i = Serialize.put (inputs i, outputs i, imports i, graph i, libID i)
--  get   = do
--            (inputs', outputs', imports', graph', libID') <- Serialize.get
--            return $ NodeDef inputs' outputs' imports' graph' libID'