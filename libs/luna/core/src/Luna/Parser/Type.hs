{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Type where

import           Flowbox.Prelude         hiding (noneOf, maybe, element, cons)
import qualified Luna.Syntax.Type        as Type
import           Luna.Syntax.Type        (Type)
import qualified Luna.Parser.Token       as Tok
import           Luna.Parser.Builder     (labeled, tuple, qualifiedPath)
import           Text.Parser.Combinators 
import           Text.Parser.Token       (braces)
import           Luna.Parser.Combinators (many1, sepBy2)

typeT       = choice [ try funcT
                     , typeSingle
                     ] <?> "type"

typeSingle  = choice [ try appT
                     , termT 
                     ] <?> "type"

termT       = choice [ try $ Tok.parens typeT 
                     , entT 
                     ] <?> "type term"

appT        = labeled (Type.App <$> appBaseT <*> many1 termT)


argListT    = braces  (sepBy2 typeT Tok.separator) <|> ((:[]) <$> typeSingle) <?> "type argument list"
funcT       = labeled (Type.Function <$> argListT <* Tok.arrow <*> typeT)

varT        = labeled (Type.Var      <$> Tok.typeVarIdent)
conT        = labeled (Type.Con      <$> qualifiedPath Tok.typeIdent)
tupleT      = labeled (Type.Tuple    <$> tuple typeT)
listT       = labeled (Type.List     <$> Tok.brackets typeT)
wildT       = labeled (Type.Wildcard <$  Tok.wildcard)

appBaseT    = choice [ varT, conT
                     ]

entT        = choice [ varT
                     , conT
                     , tupleT
                     , listT
                     , wildT
                     ]