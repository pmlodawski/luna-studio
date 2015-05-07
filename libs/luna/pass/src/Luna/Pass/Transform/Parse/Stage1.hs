---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE UndecidableInstances      #-}

module Luna.Pass.Transform.Parse.Stage1 where

import Control.Monad.Trans.Either
import Text.PrettyPrint.ANSI.Leijen (Doc, displayS, renderPretty)

import           Flowbox.Prelude        hiding (Traversal)
import           Luna.Data.ASTInfo      (ASTInfo)
import           Luna.Data.Source       (Code (Code), Source (Source), SourceReader)
import qualified Luna.Data.Source       as Source
import qualified Luna.Parser.Parser     as Parser
import qualified Luna.Parser.State      as ParserState
import           Luna.Pass              (Pass (Pass), PassCtx, PassMonad)
import           Luna.Syntax.Enum       (Enumerated, IDTag)
import           Luna.Syntax.Module     (LModule)
import qualified Luna.Syntax.Traversals as AST
import           Luna.Syntax.Unit       (Unit)

----------------------------------------------------------------------
-- Base types
----------------------------------------------------------------------

data Stage1 = Stage1

type Stage1Pass             m     = PassMonad () m
type Stage1Ctx              lab m = (Enumerated lab, PassCtx m)
type Stage1Traversal        m a b = (PassCtx m, AST.Traversal        Stage1 (Stage1Pass m) a b)
type Stage1DefaultTraversal m a b = (PassCtx m, AST.DefaultTraversal Stage1 (Stage1Pass m) a b)


------------------------------------------------------------------------
---- Pass functions
------------------------------------------------------------------------

pass :: (MonadIO m, SourceReader (Stage1Pass m) a, PassCtx m) => Pass () (Source a -> Stage1Pass m (Unit (LModule IDTag String), ASTInfo))
pass = Pass "Parser stage-1" "Parses declarations without parsing expressions" ()
       passRunner

passRunner src = do
    (Source name (Code code)) <- Source.read src
    ps <- tmpFixErrorParse code (Parser.moduleParser name Parser.defState)
    result <- lift . hoistEither $ ps
    let astinfo = view ParserState.info $ snd result
    return (fst result, astinfo)

tmpFixErrorParse a b = fixme <$> Parser.parseText a b where
    fixme = \case
        Left doc -> Left $ showWidth 200 doc
        Right r  -> Right r

showWidth :: Int -> Doc -> String
showWidth w x   = displayS (renderPretty 0.4 w x) ""
