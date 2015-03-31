---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Syntax.Graph.Tag where

import           Flowbox.Prelude
import           Luna.Syntax.Decl                (LDecl)
import           Luna.Syntax.Enum                (Enumerated (..), ID)
import qualified Luna.Syntax.Enum                as Enum
import           Luna.Syntax.Expr                (LExpr)
import qualified Luna.Syntax.Graph.Node          as Node
import           Luna.Syntax.Graph.Node.Position (Position)
import           Luna.Syntax.Module              (LModule)
import           Luna.Syntax.Pat                 (LPat)



data Tag = Empty { _idTag :: ID }
         | Node  { _idTag         :: ID
                 , _nodeID        :: Node.ID
                 , _position      :: Position
                 , _additionalPos :: Maybe Position
                 } deriving (Show, Eq)


makeLenses ''Tag


mkNode :: Node.ID -> Position -> (Maybe Position) -> Tag -> Tag
mkNode nodeID' position' additionalPos' tag' =
    Node (Enum.id tag') nodeID' position' additionalPos'

mkEmpty :: Tag -> Tag
mkEmpty = Empty . view idTag


instance Enumerated Tag where
    id = view idTag
    tag = Empty

instance Default Tag where
    def = Empty def

type TPat    = LPat Tag
type TExpr v = LExpr Tag v
type TDecl v = LDecl Tag (TExpr v)
type TModule v = LModule Tag (TExpr v)


fromEnumerated :: Enumerated e => e -> Tag
fromEnumerated = tag . Enum.id
