{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Literal where

import           Flowbox.Prelude
import           Luna.Parser.Builder     (labeled)
import qualified Luna.Parser.Token       as Tok
import qualified Luna.Syntax.Lit         as Lit
import           Text.Parser.Combinators


literal = choice [ numL, charL, stringL ]
charL   = labeled (Lit.Char   <$> Tok.charLiteral)
stringL = labeled (Lit.String <$> Tok.stringLiteral)
numL    = labeled (Lit.Number <$> Tok.numberL)

