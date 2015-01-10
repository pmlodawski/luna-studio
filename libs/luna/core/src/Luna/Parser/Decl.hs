{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Parser.Decl where

import Flowbox.Prelude hiding (cons, maybe, noneOf)

import           Text.Parser.Combinators 
import qualified Luna.Parser.Token        as Tok
import qualified Luna.Syntax.Decl         as Decl
import qualified Luna.Parser.Indent       as Indent
import           Luna.Parser.Combinators  (many1, maybe)
import           Luna.Parser.Builder      (labeled)
import qualified Luna.Parser.Type         as Type
import           Luna.Parser.Type         (typic)
import           Luna.Syntax.Foreign      (Foreign(Foreign))
import qualified Luna.Syntax.Foreign      as Foreign
import           Luna.Syntax.Name.Pattern (NamePat(NamePat), Segment(Segment))
import qualified Luna.Syntax.Name.Pattern as NamePat
import           Luna.Syntax.Arg          (Arg(Arg))
import           Luna.Parser.Builder      (qualifiedPath)
import           Luna.Syntax.Name         (tvname)
import           Luna.Parser.Pattern      (argPattern)
import qualified Luna.Syntax.Pragma       as Pragma
import qualified Luna.System.Pragma.Store as Pragma

import Luna.Parser.Struct (blockBeginFields, blockBodyOpt, blockBody', blockEnd, blockStart, blockBegin)


import           Text.Parser.Char (char, alphaNum, spaces, noneOf)


----- Imports -----

imp = Decl.Imp <$  Tok.kwImport
               <*> (qualifiedPath Tok.typeIdent <?> "import path")
               <*> ((Just <$ Tok.kwAs <*> Tok.typeIdent) <|> pure Nothing)
               <*> (blockBegin importTarget <|> pure [])
               <?> "import declaration"

importTarget =   body Decl.ImpVar Tok.varOp 
             <|> body Decl.ImpType Tok.typeIdent
             where body c p = c <$> p <*> ((Just <$ Tok.kwAs <*> p) <|> pure Nothing)


----- pragmas ------

pragma = do
    --x <- Pragma.lookupByName "test"
    Decl.Pragma <$ Tok.pragma <*> pragmaTypes <?> "pragma"

pragmaEnable  = Pragma.Enable  <$ Tok.pragmaEnable  <*> pragmaName
pragmaDisable = Pragma.Disable <$ Tok.pragmaDisable <*> pragmaName
pragmaPush    = Pragma.Push    <$ Tok.pragmaPush    <*> pragmaName
pragmaPop     = Pragma.Pop     <$ Tok.pragmaPop     <*> pragmaName

pragmaTypes = choice [ pragmaEnable, pragmaDisable, pragmaPush, pragmaPop ]

pragmaName = Tok.typeIdent <?> "pragma name"

--switchPragma 

----- type aliases ------

typeAlias = Decl.TpAls <$  Tok.kwAlias 
                       <*> (typic <?> "new type") 
                       <*  Tok.assignment 
                       <*> (typic <?> "base type")
                       <?> "type alias"


----- type wrappers ------

typeWrapper = Decl.TpWrp <$  Tok.kwType 
                         <*> (typic <?> "new type") 
                         <*  Tok.assignment 
                         <*> (typic <?> "base type")
                         <?> "type wrapper"



----- functions -----

sigVarOp = Tok.explicitName Tok.varIdent <|> Tok.operator

funcSig = try multiSig <|> singleSig

singleSig = NamePat Nothing <$> singleSigSegment <*> pure []
multiSig  = NamePat <$> maybe arg <*> multiSigSegment <*> many multiSigSegment

singleSigSegment = Segment <$> Tok.varIdent <*> many arg
multiSigSegment  = Segment <$> sigVarOp <*> many arg

arg = Arg <$> argPattern
          <*> ((Just <$ Tok.assignment <*> stage1DefArg) <|> pure Nothing)

foreign p = Foreign <$ Tok.kwForeign <*> foreignTarget <*> p 

foreignTarget =   Foreign.Haskell <$ Tok.kwFHaskell
              <|> Foreign.CPP     <$ Tok.kwFCPP

func =   Decl.Foreign <$> foreign (Decl.FFunc <$> funcDecl (char ':' *> (fromString . concat <$> stage1Body2)))
     <|> Decl.Func    <$> funcDecl (char ':' *> stage1Body2)

funcDecl body = Decl.FuncDecl <$  Tok.kwDef
                         <*> extPath
                         <*> funcSig
                         <*> outType
                         <*> body
    where extPath = ((qualifiedPath Tok.typeIdent <?> "extension path") <* Tok.accessor) <|> pure []
          outType = (Just <$> try (Tok.arrow *> typic)) <|> pure Nothing


----- classes -----

cls = Decl.Data <$> dataDecl

withBlock p = blockStart *> p <* blockEnd

rapp1 a f = f a
rapp2 a b f = f a b

dataDecl = do
    name <- Tok.kwClass *> (Tok.typeIdent <?> "class name")
    Decl.DataDecl <$> pure name 
                  <*> params
                  <**> ( try (withBlock ((rapp2) <$> constructors name <*> bodyBlock))
                         <|> ((rapp2) <$> defConsList name <*> pure [])
                       )
            <?> "class definition"
      where params         = many (tvname <$> Tok.typeVarIdent <?> "class parameter")
            defCons      n = Decl.Cons n <$> (concat <$> many fields)
            defConsList  n = ((:[]) <$> labeled (defCons $ convert n))
            constructors n =   blockBody' (labeled cons) <|> defConsList n
            bodyBlock      = blockBodyOpt $ labeled clsBody 
            clsBody        = choice [ func, cls, typeAlias, typeWrapper ] <?> "class body"


cons         = Decl.Cons <$> Tok.conIdent 
                         <*> ((concat <$> blockBeginFields fields) <|> pure [])
                         <?> "data constructor definition"


fields = do
         (names, cls) <- try ((,) <$> fieldList      <*> typed)
                         <|> ((,) <$> pure [Nothing] <*> Type.term)
         
         sequence $ fmap (labeled.pure) 
                  $ zipWith3 Decl.Field (repeat cls) names (repeat Nothing)

         where fieldList = sepBy1 (Just <$> Tok.varIdent) Tok.separator




typed = Tok.typeDecl *> Type.term



stage1DefArg = Tok.tokenBlock (many alphaNum)

stage1BodyInner = (many1 $ noneOf "\n\r")
stage1Body = (:) <$> stage1BodyInner <*> (try (spaces *> Indent.checkIndented *> stage1Body) <|> pure [[]])

stage1Body2 = ((:) <$> (try ((<>) <$> Tok.spaces <* Indent.checkIndented <*> stage1BodyInner)) <*> stage1Body2) <|> pure [[]]

--stage1Block = (++) <$> Tok.spaces <* Ident.checkIndented <*> Indent.withPos (indBlockBody p)
--stage1Body2 = (:) <$> (try (Tok.spaces <* Indent.checkIndented <* stage1BodyInner) <|> pure []) <*> stage1Body2