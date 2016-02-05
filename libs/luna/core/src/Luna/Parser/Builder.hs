{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Builder where

import           Flowbox.Prelude

import           Control.Monad.State     (get, modify)
import qualified Luna.Data.ASTInfo       as ASTInfo
import           Luna.Parser.Combinators (many1, sepBy1_ng, sepBy2)
import qualified Luna.Parser.State       as ParserState
import qualified Luna.Parser.Token       as Tok
import           Luna.Syntax.Enum        (IDTag (IDTag))
import           Luna.Syntax.Label       (Label (Label))
import           Text.Parser.Combinators

labeled p = withLabeled (const p)


withLabeled f = do
    id <- nextID
    fmap (label id) $ f id


label id = Label $ IDTag id

nextID = do
    info <- getASTInfo
    putASTInfo $ ASTInfo.incID info
    return $ info ^. ASTInfo.lastID


appID a = a <$> nextID


getASTInfo = view ParserState.info <$> get

putASTInfo info = modify (ParserState.info .~ info)


tuple         p = Tok.parens (sepBy p Tok.separator)


qualifiedPath p = sepBy1_ng p Tok.accessor <?> "qualified path"
