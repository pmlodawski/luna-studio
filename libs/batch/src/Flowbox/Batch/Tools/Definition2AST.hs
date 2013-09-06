---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Batch.Tools.Definition2AST where
-- TODO [PM] move to Luna project!

import           Flowbox.Control.Error                     
import qualified Flowbox.Luna.Network.Def.DefManager     as DefManager
import           Flowbox.Luna.Network.Def.DefManager       (DefManager)
import qualified Flowbox.Luna.Network.Def.Definition     as Definition
import           Flowbox.Luna.Network.Def.Definition       (Definition(..))
import qualified Flowbox.Luna.Network.Graph.Graph        as Graph
import qualified Flowbox.Luna.Network.Graph.Node         as Node
import           Flowbox.Luna.Network.Graph.Node           (Node(..))
import qualified Flowbox.Luna.Lib.LibManager             as LibManager
import           Flowbox.Luna.Lib.LibManager               (LibManager)
import qualified Flowbox.Luna.Lib.Library                as Library
import           Flowbox.Luna.Lib.Library                  (Library(..))
import qualified Flowbox.Luna.Parser.AST.AST             as AST
import qualified Flowbox.Luna.Parser.AST.Type            as ASTType
import           Flowbox.Luna.Type.Type                    (Type)
import qualified Flowbox.Luna.Type.Type                  as Type
import qualified Flowbox.Luna.Network.Attributes         as Attributes
import qualified Flowbox.Luna.Network.Flags              as Flags
import           Flowbox.Luna.Network.Flags                (Flags(..))
import qualified Flowbox.Luna.Network.Graph.Node         as Node
import           Flowbox.Luna.Network.Graph.Node           (Node)
import qualified Flowbox.Luna.Network.Graph.DefaultValue as DefaultValue
import qualified Flowbox.Luna.Network.Graph.Edge         as Edge
import           Flowbox.Luna.Network.Graph.Edge           (Edge(..))
import qualified Flowbox.Luna.Type.Type                  as Type
import           Flowbox.Luna.Type.Type                    (Type(..))
import qualified Flowbox.System.UniPath                  as UniPath
import           Flowbox.System.UniPath                    (UniPath)

notImplementedList = []

toAST :: Definition -> AST.Expr
toAST (Definition acls agraph aimports (Flags aio aomit) _) = case acls of 
	Class aname atypeparams aparams -> AST.Class astClass astFields astMethods where
										astClass   = (ASTType.Class aname atypeparams)
										astFields  = notImplementedList
										astMethods = notImplementedList
	_ -> AST.NOP
