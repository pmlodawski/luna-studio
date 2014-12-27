{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Pattern where

import Flowbox.Prelude
import           Text.Parser.Combinators 
import Luna.Parser.Combinators (many1, sepBy2)
import qualified Luna.Syntax.Pat    as Pat
import Luna.Parser.Builder (labeled, withLabeled)
import qualified Luna.Parser.Token       as Tok
import qualified Luna.Parser.Type as Type
import qualified Luna.Syntax.Name.Path        as NamePath
import qualified Luna.Parser.State            as ParserState
import qualified Luna.Data.Namespace.State    as Namespace
import           Luna.Data.StructInfo         (OriginInfo(OriginInfo))
import           Luna.Parser.Literal          (literal)


pattern    = choice [ try implTupleP
                    , patCon
                    ]

patTup     = pattern <|> (labeled (Pat.Tuple <$> pure []))

patCon     = choice [ try appP
                    , termP
                    ]

argPattern = termBase Type.termT

termP      = termBase Type.typeT

termBase t = choice [ try (labeled (Pat.Grouped <$> Tok.parens patTup))
                    , try (labeled (Pat.Typed   <$> entP <* Tok.typeDecl <*> t))
                    , entP
                    ]
              <?> "pattern term"

varP       = withLabeled $ \id -> do
                name <- Tok.varIdent
                let np = NamePath.single name
                path <- ParserState.getModPath
                Namespace.regVarName (OriginInfo path id) np
                return $ Pat.Var (fromText name)

litP       = labeled (Pat.Lit         <$> literal)
implTupleP = labeled (Pat.Tuple       <$> sepBy2 patCon Tok.separator)
wildP      = labeled (Pat.Wildcard    <$  Tok.wildcard)
recWildP   = labeled (Pat.RecWildcard <$  Tok.recWildcard)
conP       = labeled (Pat.Con         <$> Tok.conIdent)
appP       = labeled (Pat.App         <$> conP <*> many1 termP)

entP = choice [ varP
              , litP
              , wildP
              , recWildP
              , conP
              ]