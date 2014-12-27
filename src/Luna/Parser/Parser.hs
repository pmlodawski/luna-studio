---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE DeriveDataTypeable        #-}

{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings          #-}


--{-# LANGUAGE OverlappingInstances #-}

module Luna.Parser.Parser where


import           Control.Applicative
import           Control.Exception            (bracket)
import           Flowbox.Control.Monad.State  hiding (mapM_, (<$!>), join, mapM, State)
import qualified Data.ByteString              as B
import qualified Data.ByteString.UTF8         as UTF8
import           Data.CharSet.ByteSet         as S
import           Data.Default
import           Flowbox.Prelude              hiding (noneOf, maybe, element, cons)
import qualified Flowbox.Prelude              as Prelude
import qualified Luna.Data.ASTInfo            as ASTInfo
import qualified Luna.Parser.Token            as Tok
import qualified Luna.Parser.State            as ParserState
import           Luna.Parser.State            (ParserState)
import           System.Environment           (getArgs)
import           System.IO                    (IOMode (ReadMode), hClose, openFile)
import           System.IO                    (stdout)
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.PrettyPrint.ANSI.Leijen (displayIO, linebreak, renderPretty)
import qualified Text.PrettyPrint.ANSI.Leijen as Leijen
import           Text.RawString.QQ
import           Text.Trifecta                hiding (parseFromFile, parseByteString, parseString)
import qualified Text.Trifecta                as Trifecta
import           Text.Trifecta.Delta          as Delta
import qualified Luna.Data.Config             as Config
import qualified Luna.Pragma.Pragma           as Pragma
import           Luna.Pragma.Pragma           (IsPragma)
import           Data.Typeable
import           Data.String.Utils            (join)
import           Luna.Parser.Combinators
import           Text.Parser.Expression
import           Text.Parser.LookAhead
import           Data.Char                    (isSpace)
import qualified Data.ByteString as ByteStr
import           Luna.Syntax.Name.Path        (NamePath(NamePath))
import qualified Luna.Syntax.Name.Path        as NamePath
import qualified Luna.Syntax.Name.Pattern     as NamePat
import           Luna.Syntax.Name.Pattern     (NamePat(NamePat), Segment(Segment))
import qualified Luna.Syntax.Name             as Name
import           Luna.Syntax.Name             (VName, vname, TName, tname, TVName, tvname)

--import qualified Luna.Data.Namespace          as Namespace
import qualified Luna.Data.Namespace.State    as Namespace
import qualified Luna.Data.StructInfo          as StructInfo
import           Luna.Data.StructInfo         (OriginInfo(OriginInfo))
import qualified Luna.Syntax.AST              as AST
import qualified Luna.Syntax.Traversals       as AST
import qualified Data.Maps                    as Map
import           Data.Maybe                   (isJust, fromJust)
import qualified Flowbox.Data.MapForest       as MapForest
import qualified Data.List                    as List
import qualified Luna.Parser.Pragma           as Pragma

import           Text.EditDistance            --(defaultEditCosts, levenshteinDistance, EditCosts, Costs(..))
import           Text.PhoneticCode.Phonix     (phonix)
import           Data.Function                (on)
import           Data.List                    (sort, sortBy)
import           Luna.Syntax.Name.Path (QualPath(QualPath))

import qualified Data.IntMap  as IntMap

--import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet

import qualified Luna.Parser.Indent as Indent


import qualified Luna.Syntax.Expr as Expr
import           Luna.Syntax.Expr (LExpr, Expr(Expr))


import qualified Luna.Syntax.Decl   as Decl
import           Luna.Syntax.Decl   (LDecl, Field(Field))
import qualified Luna.Syntax.Module as Module
import           Luna.Syntax.Module (Module(Module), LModule)
import           Luna.Syntax.Unit   (Unit(Unit))
import qualified Luna.Syntax.Label  as Label
import           Luna.Syntax.Label  (Label(Label))
import qualified Luna.Syntax.Type   as Type
import           Luna.Syntax.Type   (Type)
import qualified Luna.Syntax.Pat    as Pat
import           Luna.Syntax.Pat    (LPat, Pat)
import           Luna.Syntax.Arg    (Arg(Arg))
import qualified Luna.Syntax.Native as Native

import qualified Luna.Syntax.Enum       as Enum
import           Luna.Syntax.Enum       (Enumerated, IDTag(IDTag))
import qualified Luna.Syntax.Unit       as Unit


import qualified Data.TypeLevel.Set as TLSet
import           Data.Tuple.Select

import qualified Data.Text.Lazy.Encoding as Text

import           Luna.Syntax.Foreign (Foreign(Foreign))
import qualified Luna.Syntax.Foreign as Foreign

import Luna.Parser.Type
import Luna.Parser.Pattern
import Luna.Parser.Literal
import Luna.Parser.Struct
import Luna.Parser.Term

import Luna.Parser.Builder (labeled, label, nextID, qualifiedPath, withLabeled)


mtry p = try p <|> pure mempty

vName = Name.V <$> Tok.varOp
tName = Name.T <$> Tok.typeIdent

anyName = vName <|> tName











--extensionPath   = (,) <$> (((qualifiedPath Tok.typeIdent <?> "extension path") <* Tok.accessor) <|> pure [])
--                      <*> (namePattern <?> "function name")

--namePattern =   (NamePat.single <$> varOp)
--            <|> Tok.parens (NamePat.close <$> (NamePat.multi <$> Tok.varIdent <*> many1 namePatSeg))

--namePatSeg =   (NamePat.Token <$> Tok.varIdent)
--           <|> (NamePat.Hole  <$  Tok.nameWildcard)


--namePattern =   (NamePath.single <$> varOp)
--            <|> Tok.parens (NamePath.multi <$> Tok.varIdent <*> many1 Tok.varIdent)


argList       p = try (sepBy2 p Tok.separator) <|> many p <?> "argument list"
argList'      p = braces (sepBy2 p Tok.separator) <|> ((:[]) <$> p) <?> "argument list"
list          p = Tok.brackets (sepBy p Tok.separator)
anyIdent        = choice [ Tok.varIdent, Tok.typeIdent ]












element m = do
    id <- nextID
    ParserState.registerID id
    ast <- m id
    --ParserState.registerAST id ast
    return ast



container m = element $ \id -> ParserState.withNewScope id $ m id




--regVarName m id = do
--    ast <- m id
--    ParserState.regVarName id (AST.name ast)
--    return ast


--nameTok p = element $ regVarName $ \id -> p <*> pure id

tok p = element $ \id -> p <*> pure id

-- Parser translation unit.
-- Provides a global namespace when parsing module, expression etc.
unit p = do
    --FIXME[WD] : change id to datatype
    let id = -666 
    --id <- nextID
    --Unit id <$> ParserState.withNewScope id p
    ParserState.withNewScope id p


-----------------------------------------------------------
-- Definitions
-----------------------------------------------------------


pUnit p = Unit <$> labeled p


decl = choice [ imp, func, cls, typeAlias, typeWrapper ]

----- Modules -----

pModule name path = do
    let qpath = (QualPath path name)
    ParserState.setModPath qpath
    Module qpath <$> Indent.withPos (moduleBlock $ labeled moduleBody)
    where moduleBody = decl <?> "module body"

----- Imports -----

imp = Decl.Imp <$  Tok.kwImport
               <*> (qualifiedPath Tok.typeIdent <?> "import path")
               <*> ((Just <$ Tok.kwAs <*> Tok.typeIdent) <|> pure Nothing)
               <*> (blockBegin importTarget <|> pure [])
               <?> "import declaration"

importTarget =   body Decl.ImpVar Tok.varOp 
             <|> body Decl.ImpType Tok.typeIdent
             where body c p = c <$> p <*> ((Just <$ Tok.kwAs <*> p) <|> pure Nothing)


----- type aliases ------

typeAlias = Decl.TpAls <$  Tok.kwAlias 
                       <*> (typeT <?> "new type") 
                       <*  Tok.assignment 
                       <*> (typeT <?> "base type")
                       <?> "type alias"


----- type wrappers ------

typeWrapper = Decl.TpWrp <$  Tok.kwType 
                         <*> (typeT <?> "new type") 
                         <*  Tok.assignment 
                         <*> (typeT <?> "base type")
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
          outType = (Just <$> try (Tok.arrow *> typeT)) <|> pure Nothing


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
                         <|> ((,) <$> pure [Nothing] <*> termT)
         
         sequence $ fmap (labeled.pure) 
                  $ zipWith3 Field (repeat cls) names (repeat Nothing)

         where fieldList = sepBy1 (Just <$> Tok.varIdent) Tok.separator




typed = Tok.typeDecl *> termT



stage1DefArg = Tok.tokenBlock (many alphaNum)

stage1BodyInner = (many1 $ noneOf "\n\r")
stage1Body = (:) <$> stage1BodyInner <*> (try (spaces *> Indent.checkIndented *> stage1Body) <|> pure [[]])

stage1Body2 = ((:) <$> (try ((<>) <$> Tok.spaces <* Indent.checkIndented <*> stage1BodyInner)) <*> stage1Body2) <|> pure [[]]

--stage1Block = (++) <$> Tok.spaces <* Ident.checkIndented <*> Indent.withPos (indBlockBody p)
--stage1Body2 = (:) <$> (try (Tok.spaces <* Indent.checkIndented <* stage1BodyInner) <|> pure []) <*> stage1Body2
--dokonczyc bo nie ma wciec



-----------------------------------------------------------
-- Patterns
-----------------------------------------------------------



----------------------------------------------------------------------
-- Literals
----------------------------------------------------------------------

-- FIXME [wd]: last parsed char is poorly written with '_' workaround when no char is available



-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------



----------------------------------------------------------------------
-- Code segments
----------------------------------------------------------------------
--indBlockBody   p = many1 (Indent.checkIndentedOrEq *> p <* indBlockSpaces)

--indBlockSpaces   = try (Tok.spaces <* Indent.checkIndent) <|> pure mempty




-----------------------------------------------------------
-- Utils
-----------------------------------------------------------

parserName = "Luna Compiler"

run p st = evalStateT (Indent.parser p) st

handleResult r = case r of
    Failure e -> Left e
    Success a -> Right a

bundleResult p = (,) <$> p <*> get

end = (Tok.spaces <?> "") <* (eof <?> "")

upToEnd p = Tok.spaces *> p <* end

renderErr e = renderPretty 0.8 80 $ e Leijen.<> linebreak

-----------------------------------------------------------
-- Pragmas
-----------------------------------------------------------

appConf = Config.registerPragma (undefined :: Pragma.TabLength)
        . Config.registerPragma (undefined :: Pragma.AllowOrphans)
        . Config.registerPragma (undefined :: Pragma.ImplicitSelf)

-- FIXME[wd]: logika powina byc przeniesiona na system pluginow
defConfig = appConf def
-- FIXME[wd]: debugowo ustawione wartosci typow
emptyState = def :: ParserState ()
defState  = emptyState & ParserState.conf .~ defConfig


appSt = ParserState.conf %~ appConf

--st = def {State._conf = conf}

-----------------------------------------------------------
-- Section parsing
-----------------------------------------------------------
-- Usage example: parseExpr (fileFeed "test.txt")
parseGen p st = run (bundleResult (unit p)) st
parseGen2 p st = run (bundleResult p) st

--moduleParser modPath = parseGen (upToEnd $ func)
moduleParser modPath = parseGen (upToEnd $ pUnit $ pModule (last modPath) (init modPath))
--exprParser           = parseGen (upToEnd expr)
exprBlockParser      = parseGen (upToEnd $ indBlock expr)
exprBlockParser2     = parseGen2 (upToEnd $ indBlock expr)
exprParser2          = parseGen2 (upToEnd expr)
--patternParser        = parseGen (upToEnd pattern)
--typeParser           = parseGen (upToEnd typeT)

-----------------------------------------------------------
-- Input utils
-----------------------------------------------------------

parserDelta name = Directed (UTF8.fromString name) 0 0 0 0

parseFromByteString = Trifecta.parseByteString

parseFromText p delta txt = Trifecta.parseByteString p delta (convert $ Text.encodeUtf8 txt)

parseFromString p delta input = parseFromByteString p delta (UTF8.fromString input)

parseFromFile p delta path = do
  s <- liftIO $ ByteStr.readFile path
  return $ parseFromByteString p delta s

parseFile       path  p = handleResult <$> parseFromFile       p (parserDelta parserName) path
parseString     input p = handleResult  $  parseFromString     p (parserDelta parserName) input
parseByteString input p = handleResult  $  parseFromByteString p (parserDelta parserName) input

parseByteString2 p input = handleResult  $  parseFromByteString p (parserDelta parserName) input
parseText2 p input = handleResult  $  parseFromText p (parserDelta parserName) input
                --data AliasAnalysis = AliasAnalysis

                --traverseM        = AST.traverseM        AliasAnalysis
                --defaultTraverseM = AST.defaultTraverseM AliasAnalysis

testme ast st = ast -- runState (traverseM ast) st


                ----type AACtx m lab e a conf v = (Enumerated lab, TLSet.Lookup conf (Pragma.Pragma Pragma.AllowOrphans),
                ----                              MonadState (State a e v conf) m, Show conf, Show v, Show e, Show a, Functor m) 

                --type AACtx m lab e s conf v = (Enumerated lab, MonadState (State s e v conf) m, Applicative m)

                --instance (AACtx m lab e s conf v, AST.Traversal AliasAnalysis m a a)
                --    => AST.Traversal AliasAnalysis m (LModule lab a) (LModule lab a) where
                --    traverseM _ = aatest

                --instance (AACtx m lab e s conf v, AST.Traversal AliasAnalysis m a a)
                --      => AST.Traversal AliasAnalysis m (LDecl lab a) (LDecl lab a) where
                --    traverseM _ = traverseDecl

                --instance AACtx m lab e s conf v
                --      => AST.Traversal AliasAnalysis m (LPat lab) (LPat lab) where
                --    traverseM _ = registerPat



                --aaunit (Unit mod) = Unit <$> aatest mod

                --aatest mod@(Label lab (Module path name body)) = State.withNewScope id continue
                --        where continue =  registerDecls body
                --                       *> defaultTraverseM mod
                --              id       = Enum.id lab


                --registerDecls decls =  mapM registerHeaders  decls
                --                    *> mapM registerDataDecl decls


                --registerDataDecl (Label lab decl) = case decl of
                --    Decl.Data     name _ cons defs   -> State.withNewScope id (registerDecls defs) *> pure ()
                --    _                                -> pure ()
                --    where id = Enum.id lab

                --registerHeaders (Label lab decl) = case decl of
                --    Decl.Function _ name inputs _ _  -> State.regVarName id (view NamePath.base name)
                --                                     <* State.withNewScope id (traverseM inputs)
                --    Decl.Data     name _ cons _      -> State.regTypeName id (Name.fromName name) 
                --                                     <* mapM_ registerCons cons
                --    _                                -> pure ()
                --    where id = Enum.id lab

                --registerPat p@(Label lab pat) = case pat of
                --    Pat.Var         name       -> State.regVarName id (Name.fromName name) *> continue
                --    _                          -> continue
                --    where id = Enum.id lab
                --          continue = defaultTraverseM p 

                --registerCons (Label lab (Decl.Cons name fields)) = State.regVarName (Enum.id lab) (Name.fromName name)


                --traverseDecl d@(Label lab decl) = case decl of
                --    Decl.Function path name inputs output body -> State.withNewScope id $ defaultTraverseM d
                --    _ -> continue
                --    where id       = Enum.id lab
                --          continue = defaultTraverseM d


                --traverseDecl2Pass (Label lab decl) = fmap (Label lab) $ case decl of
                --    Decl.Function path name inputs output body -> do
                --        subAST  <- subparse (unlines body)
                --        inputs' <- mapM subparseArg inputs
                --        return $ Decl.Function path name inputs' output subAST
                --    Decl.Data        name params cons defs -> return $ Decl.Data        name params [] []
                --    Decl.Import      path rename targets   -> return $ Decl.Import      path rename targets
                --    Decl.TypeAlias   dst src               -> return $ Decl.TypeAlias   dst src
                --    Decl.TypeWrapper dst src               -> return $ Decl.TypeWrapper dst src
                --    where id = Enum.id lab
                --          subparse expr = do 
                --              result <- State.withScope id (parseString expr <$> (exprBlockParser2 <$> get))
                --              case result of
                --                  Left e      -> fail   $ show e
                --                  Right (e,_) -> return $ e
                --          -- FIXME [wd]: inny parser powinine parsowac argumenty poniewaz nie zawieraja wielu linii i nie moga zawierac wielu exproessionow!
                --          --             zatem wyciaganie pierwszego elementu jest szybkim obejsciem
                --          subparseArg (Arg pat val) = Arg pat . (fmap (!!0)) <$> mapM subparse val 




--registerClassHeaders (Label lab decl) = case decl of
--    Decl.Data       cls cons _ _ -> register' id cls cons
--    where id = Enum.id lab
--          register' id cls cons = State.regTypeName name id -- <* mapM registerConsHeaders cons
--                                  where name = view Type.name cls


    --instance (MonadState (State.State a) m, Enumerated lab) => AST.Traversal AliasAnalysis m (Label lab (Module f e)) where
    --    traverse base x@(Label lab m) = State.withNewScope id continue
    --        where continue = AST.defaultTraverse base x
    --              id       = Enum.id lab

    --instance (MonadState (State.State a) m, Enumerated lab) 
    --         => AST.Traversal AliasAnalysis m (Label lab (Decl.Decl f e)) where
    --    traverse base x@(Label lab ast) = case ast of
    --        --Decl.Function path name inputs output body -> State.regVarName id (view NamePath.base name) *> State.withNewScope id continue
    --        --_                                          -> continue
    --        _                                          -> undefined
    --        where continue = AST.defaultTraverse base x
    --              id       = Enum.id lab


--s2Decl d = case Label.element d of
--    Decl.Function path name inputs output body -> State.regVarName id (view Name.base name) *> 
--    where id = Label.label d

--    | Function    { _path    :: Path    , _fname    :: NamePath  , _inputs  :: [Arg f e]   , _output :: Maybe (RType f) , _body :: [e] }



    --vaMod :: Module -> VAPass StructInfo
    --vaMod el@(Module.Module id cls imports classes typeAliases typeDefs fields methods modules) = do
    --    regModule el
    --    withScope id $ regVarName name id *> continue
    --    getAliasInfo
    --    where name     = el ^. Module.cls ^. Type.name
    --          continue =  pure ()
    --                   -- -- <* mapM registerDataCons classes -- register just data constructors before functions
    --                   <* mapM registerFuncHeaders methods
    --                   <* mapM registerClassHeaders classes

    --                   <* vaType cls
    --                   <* fexpMap imports
    --                   <* fexpMap classes -- register class functions before data member functions
    --                   <* fexpMap typeAliases
    --                   <* fexpMap typeDefs
    --                   <* fexpMap fields
    --                   <* fexpMap methods
    --                   <* fmodMap modules
    --          fexpMap  = mapM vaExpr
    --          fmodMap  = mapM vaMod


    ----registerDataCons :: Expr.Expr -> VAPass ()
    ----registerDataCons el = VAState.regExpr el *> case el of
    ----    --Expr.Data       {} -> withID continue
    ----    --Expr.ConD       {} -> regParentVarName name id *> continue
    ----    _                  -> continue
    ----    where continue = Expr.traverseM_ registerDataCons vaType vaPat vaLit el
    ----          withID   = VAState.withID (el ^. Expr.id)
    ----          id       = el ^.  Expr.id
    ----          name     = el ^.  Expr.name

    --registerClassHeaders :: Expr.Expr -> VAPass ()
    --registerClassHeaders cls = case cls of
    --    Expr.Data       id cls cons _ _ -> register' id cls cons
    --    Expr.DataNative id cls cons _ _ -> register' id cls cons
    --    where register' id cls cons = regTypeName name id <* mapM registerConsHeaders cons
    --                                  where name = view Type.name cls

    --registerConsHeaders :: Expr.Expr -> VAPass ()
    --registerConsHeaders (Expr.ConD id name fields) = regVarName name id



    --registerFuncHeaders :: Expr.Expr -> VAPass ()
    --registerFuncHeaders el = regExpr el *> case el of
    --    Expr.Function   id _ name _ _ _ -> regVarName (Name.unified name) id
    --    where continue = Expr.traverseM_ registerFuncHeaders vaType vaPat vaLit pure el