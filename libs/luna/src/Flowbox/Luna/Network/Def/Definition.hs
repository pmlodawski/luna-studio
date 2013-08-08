---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Network.Def.Definition(
    Definition(..),
    ID,
    empty,
    noImports
) where

import qualified Flowbox.Luna.Type.Type           as Type
import           Flowbox.Luna.Type.Type             (Type)
import qualified Flowbox.Luna.Lib.Library         as Library
import           Flowbox.Luna.Network.Graph.Graph   (Graph)
import qualified Flowbox.Luna.Network.Graph.Graph as Graph
import qualified Flowbox.Luna.Network.Flags       as Flags
import           Flowbox.Luna.Network.Flags         (Flags)
import qualified Flowbox.Luna.Network.Attributes  as Attributes
import           Flowbox.Luna.Network.Attributes    (Attributes)
import           Flowbox.Luna.Network.Path.Import   (Import)


data Definition = NotLoaded
             | Definition {
                   cls        :: Type,
                   graph      :: Graph,
                   imports    :: [Import],
                   flags      :: Flags, 
                   attributes :: Attributes,
                   libID      :: Library.ID
               } deriving (Show)

type ID   = Int

empty :: Definition
empty = Definition Type.Undefined Graph.empty noImports Flags.empty Attributes.empty (-1)

noImports :: [Import]
noImports = []

--make :: Type -> Library.ID -> Definition
--make t lib = Definition t Graph.empty Flags.empty Attributes.empty lib



------------------------- INSTANCES -------------------------

--instance Serialize Definition where
--  put i = Serialize.put (inputs i, outputs i, imports i, graph i, libID i)
--  get   = do
--            (inputs', outputs', imports', graph', libID') <- Serialize.get
--            return $ Definition inputs' outputs' imports' graph' libID'