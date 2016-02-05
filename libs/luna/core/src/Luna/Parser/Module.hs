{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Module where

import           Flowbox.Prelude         hiding (cons, maybe, noneOf)

import           Luna.Parser.Builder     (labeled)
import           Luna.Parser.Decl        (cls, foreigns, func, imp, pragma, typeAlias, typeWrapper)
import qualified Luna.Parser.Indent      as Indent
import qualified Luna.Parser.State       as ParserState
import           Luna.Parser.Struct      (moduleBlock)
import           Luna.Syntax.Module      (Module (Module))
import           Luna.Syntax.Name.Path   (QualPath (QualPath))
import           Luna.Syntax.Unit        (Unit (Unit))
import           Text.Parser.Combinators

pModule qpath = do
    ParserState.setModPath qpath
    Module qpath <$> Indent.withPos (moduleBlock $ labeled moduleBody)
    where moduleBody = decl <?> "module body"

pUnit p = Unit <$> labeled p

decl = choice [ imp, func, cls, typeAlias, typeWrapper, pragma, foreigns]



unit p = do
    --FIXME[WD] : change id to datatype
    let id = -777
    --id <- nextID
    --Unit id <$> ParserState.withNewScope id p
    ParserState.withNewScope id p
