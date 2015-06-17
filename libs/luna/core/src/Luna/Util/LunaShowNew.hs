---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE DataKinds                 #-}
module Luna.Util.LunaShowNew where


import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as Text
import           Data.Text.Lazy.Builder       (fromLazyText, toLazyText)
import qualified Data.Text.Lazy.Builder       as Text
import           Prelude                      (sequence)

import           Luna.Pass                    (Pass (Pass), PassMonad)
import           Flowbox.Prelude
import           Luna.Syntax.Label            (Label (Label))
import qualified Luna.Syntax.Label            as L
import qualified Luna.Syntax.Unit             as U
import           Luna.Syntax.Module           (Module (Module))
import           Luna.Syntax.Unit             (Unit (Unit))
import           Luna.Syntax.Name.Path        (QualPath)
import qualified Luna.Syntax.Name.Path        as Q
import qualified Luna.Syntax.Decl             as D
import           Data.Text.CodeBuilder.Builder
import qualified Data.Text.CodeBuilder.Builder as Builder
import           Data.Text.CodeBuilder.Doc     (between, line, nested, (</>))
import qualified Data.Text.CodeBuilder.Doc     as Doc
import           Data.Text.CodeBuilder.Tok     (Prec (Top), Tok (Tok))
import qualified Data.Text.CodeBuilder.Tok     as Tok
import qualified Luna.Syntax.Name.Pattern      as Pattern
import qualified Luna.Syntax.Arg               as Arg
import qualified Luna.Syntax.Lit               as Lit
import qualified Luna.Syntax.Lit.Number        as Num
import qualified Luna.Syntax.Pat               as Pat
import qualified Luna.Syntax.Name              as Name
import           Luna.Syntax.Name              ()
import qualified Luna.Syntax.Expr              as E               
import qualified Luna.Syntax.Arg               as Arg
import qualified Luna.Syntax.Type              as T
import qualified Luna.Syntax.Foreign           as Fg
import           Luna.Syntax.Pragma            (Pragma(Enable, Disable, Push, Pop))

----------------------------------------------------------------------
-- Basic types and classes
----------------------------------------------------------------------
data AST2Source = AST2Source
type PassResult m   = PassMonad HSIndent m

class Generator a s where
    generate :: a -> Builder s Tok

class AutoGen a b where
    gen :: a -> b


------------------------------------------------------------------------
---- Basic Pass logic
------------------------------------------------------------------------
--pass :: (Generator v HSIndent, Monad m) => Pass HSIndent (v -> PassResult m Text)
--pass = Pass "AST2Source" "This is not compiler pass but it is usefull for now " HSIndent genUnit

--genUnit :: (Monad m, Generator v HSIndent) => v -> m Text
--genUnit ast = return . toLazyText . runMeI . generate $ ast
 
------------------------------------------------------------------------
---- Instances and Utils
------------------------------------------------------------------------
lunaShow :: (Generator v HSIndent) => v -> String
lunaShow = Text.unpack . toLazyText . runMeI . generate

runMeI :: Builder HSIndent Tok -> Text.Builder
runMeI = renderCode HSIndent

--instance Convertible Text (Builder s Tok) where
--    safeConvert = Right . pure . fromText

--instance (s~s') => Convertible (Builder s Tok) (Builder s' Tok) where
--    safeConvert = Right

--instance Convertible String (Builder s Tok) where
--    safeConvert = Right . pure . fromString



app' a b  = app (convert a) (convert b)
app'' a b = appWithoutSpace (convert a) (convert b)

apps' base = foldl app' (convert base)
apps'' base = foldl app'' (convert base)

accesor acc src = apps'' src [".", acc]
assign' a b = apps' a ["=", b]

appWithoutSpace :: Builder s Tok -> Builder s Tok -> Builder s Tok
appWithoutSpace = appWith "" L 10
----------------------------------------------------------------------
-- Render Styles
----------------------------------------------------------------------

data HSIndent  = HSIndent  deriving (Show)
data Block = Block [Tok]
data ConsBlock = ConsBlock [Text] [Tok]

block :: Render s Block => [Builder s Tok] -> Builder s Tok
block items = renderStyled . Block =<< sequence items

consBlock items ders = renderStyled . ConsBlock ders =<< sequence items

instance Render HSIndent Block where
    render _ (Block items) = Tok Top . nested
                                     . foldl (</>) mempty
                                     $ fmap (view Tok.doc) items
instance Render HSIndent ConsBlock where
    render _ (ConsBlock ders items) = Tok Top $ nested (genDeriving ders cons)
        where cons = foldl concat mempty
                   . zip ("= " : repeat "| ")
                   $ fmap (view Tok.doc) items
              concat a (sep,b) = a <> line <> sep <> b
genDeriving ders = case ders of
    [] -> id
    _  -> (<> (line <> "deriving "
                    <> Doc.parensed (mjoin "," $ fmap fromText ders)))

instance Monoid (Pat.VName )

----------------------------------------------------------------------
-- Generator Instances
----------------------------------------------------------------------

instance (AutoGen a b, out~[b]) => AutoGen [a] out where
    gen = fmap gen

instance (Generator a s, out~Builder s Tok) => AutoGen a out where
    gen = generate

-- Main generators:
instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (Unit v) s where
    generate (Unit content) = generate content

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (Module lab v) s where
    generate (Module _ elems) = {-block $ (fmap generate elems)-} mjoin "\n\n" (fmap generate elems)

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (D.Decl lab v) s where
    generate = \case
        D.Func a           -> generate a
        D.Data decl        -> generate decl
        D.TpAls left right -> "alias" `app` generate left `app` "=" `app` generate right
        D.TpWrp _ _        -> "Decl.TpWrp"
        D.Foreign fg       -> generate fg
        D.Imp imp          -> generate imp
        D.Pragma pragma    -> generate pragma
        
instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (L.Label e v) s where
    generate (L.Label e v) = generate v

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (E.Expr lab v) s where
      generate (E.Accessor ac src) = (generate ac) `accesor` (generate src)
      generate (E.App exprApp)     = generate exprApp          
      generate (E.Assignment l r)  = assign' (generate l) (generate r)          
      generate (E.Case cnd pes)    = "case" `app` generate cnd `appWithoutSpace` ":" `app` (block $ (fmap generate pes))
      generate (E.Cons con)        = fromText (toText con) :: Builder s Tok 
      generate (E.Curry curried)   = "@" `appWithoutSpace` generate curried
      generate (E.Decl decl)       = generate decl
      generate (E.Grouped group)   = "(" `appWithoutSpace` (sbox $ generate group) `appWithoutSpace` ")"
      generate (E.Lambda inp _ bd) = (fmap (mjoin ",") $ sequence $ fmap generate inp) `appWithoutSpace` ":" `app` (block $ (fmap generate bd))
      generate (E.List list)       = generate list 
      generate (E.Lit lLit)        = generate lLit
      generate (E.Meta _)          = "Meta" 
      generate (E.Native _)        = "Native"
      generate (E.RecUpd vn fs)    = (fromString (toString vn) :: Builder s Tok) `appWithoutSpace` "." `appWithoutSpace` generate (head fs)
      generate (E.Tuple elems)     = fmap (mjoin ",") $ sequence $ fmap generate elems
      generate (E.Typed pat cls)   = generate pat `appWithoutSpace` "::" `appWithoutSpace` generate cls
      generate (E.Var var)         = generate var
      generate (E.Wildcard)        = "_"

instance (Render s ConsBlock, Render s Block) => Generator (Pat.Pat a) s where
    generate (Pat.App src args)     = generate src `app` (fmap (mjoin "") $ sequence $ fmap generate args)
    generate (Pat.Con con)          = fromText (toText con) :: Builder s Tok
    generate (Pat.Grouped group)    = "(" `appWithoutSpace` generate group `appWithoutSpace` ")"
    generate (Pat.Lit lit)          =  generate lit
    generate (Pat.RecWildcard)      =  "_"
    generate (Pat.Tuple elems)      = fmap (mjoin ",") $ sequence $ fmap generate elems
    generate (Pat.Typed pat cls)    = generate pat `appWithoutSpace` "::" `appWithoutSpace` generate cls
    generate (Pat.Var var)          = fromText (toText var) :: Builder s Tok
    generate (Pat.Wildcard)         =  "_"

instance Generator Lit.Lit s where
    generate (Lit.Char c)   = fromString ("'" ++ [c] ++ "'") :: Builder s Tok
    generate (Lit.String s) = fromString ("\"" ++ s ++ "\"") :: Builder s Tok
    generate (Lit.Number n) = fromString (Num.lunaShow n )   :: Builder s Tok

instance Generator (E.Variable v) s where
    generate (E.Variable vpath _) = fromString (toString vpath) :: Builder s Tok

instance Generator (Name.NameBaseP) s where
    generate ac = (fromText . toText . unwrap $ ac) :: Builder s Tok

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (E.List v) s where
    generate (E.SeqList elems)                 = list (fmap generate elems)
    generate (E.RangeList (E.Linear _ _))      = "[TD]Linear list not supported yet."
    generate (E.RangeList (E.Geometric l r _)) = "[" `appWithoutSpace` generate l 
                                                     `appWithoutSpace` ".." 
                                                     `appWithoutSpace` generate r 
                                                     `appWithoutSpace` "]"

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (Arg.Arg lab v) s where
    generate (Arg.Arg pat Nothing)    = generate pat
    generate (Arg.Arg pat (Just val)) = generate pat `appWithoutSpace` "=" `appWithoutSpace` generate val

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (E.FieldUpd lab v) s where
    generate (E.FieldUpd sel expr) = (fromString (toString . head $ sel) :: Builder s Tok) `app` "=" `app` generate expr


instance (Render s ConsBlock, Render s Block) => Generator (T.Type lab) s where
    generate = \case
        T.App src args      -> generate src `app` (fmap (mjoin "") $ sequence $ fmap generate args)
        T.Con tn            -> fromText (Text.intercalate " " (fmap toText tn)) :: Builder s Tok
        T.Function ins out  -> ((mjoin "->") $ fmap generate ins) `appWithoutSpace` "->" `appWithoutSpace` generate out
        T.List elem         -> "[" `appWithoutSpace` generate elem `appWithoutSpace` "]"
        T.Meta _            -> "Meta"
        T.Tuple elems       ->  (fmap (mjoin ",") $ sequence $ fmap generate elems)
        T.Var vn            -> (fromString (toString vn) :: Builder s Tok)
        T.Wildcard          -> "_"
    

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (D.DataDecl lab v) s where
    generate (D.DataDecl name elems cons decls) = "class" `app` nameBuilder `app` params `appWithoutSpace` ":" `app` bodyBuilder
         where params      = fromText (Text.intercalate " " (fmap toText elems)) :: Builder s Tok
               nameBuilder = (fromText . toText $ name :: Builder s Tok)
               consBuilder = fmap generate cons
               declBuilder = fmap generate decls
               bodyBuilder = block $ consBuilder ++ declBuilder

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (E.Match lab v) s where
    generate (E.Match left body) = generate left `appWithoutSpace` ":" `app` (block $ (fmap generate body))

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (E.ExprApp lab v) s where
    generate (Pattern.NamePat prx base sgs) = generate prx `app` (sbox $ generate base)

instance (Generator e s, Generator v s, Render s ConsBlock, Render s Block) => Generator (Pattern.Segment e v) s where
    generate (Pattern.Segment base args)= apps' (generate base) (fmap (sbox . generate) args)

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (E.AppArg v) s where
    generate (E.AppArg name val) = generate val

instance Generator () HSIndent where 
 generate _ = "Internal Error of AST2Source.hs"


instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (D.FuncDecl lab v [v]) s where
    generate (D.FuncDecl path sig out body) = "def" `app` (sbox $ generate sig `appWithoutSpace` ":" `app` sbox (block $ fmap generate body))

instance (Generator Text s,Generator v s, Render s ConsBlock, Render s Block) => Generator (D.FuncDecl lab v Text) s where
    generate (D.FuncDecl path sig out body) = "def" `app` generate sig `appWithoutSpace` ":" `app` generate body

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (Pattern.ArgPat lab v) s where
     generate (Pattern.NamePat prx base sgs) = generate prx `app` generate base

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (D.Cons lab v) s where
    generate = \case
        D.Cons tn [] -> tnBuilder tn
        D.Cons tn fields -> tnBuilder tn `appWithoutSpace` ":" `app`  (block $ fmap generate fields) 
      where tnBuilder tn =  (fromString (toString tn) :: Builder s Tok)

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (D.Field lab v) s where
    generate (D.Field  tp (Just nm) _) = (fromString (toString nm) :: Builder s Tok) `appWithoutSpace` "::" `appWithoutSpace` generate tp
    generate _ = "Internal error in AST2Source.hs, Generator D.Field s"

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (Fg.Foreign v) s where
    generate = \case
        Fg.Foreign Fg.Haskell fg -> "foregin haskell" `app` generate fg
        Fg.Foreign Fg.CPP fg     -> "foreign cpp" `app` generate fg

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (D.ForeignDecl lab v) s where
    generate = \case
        D.FData dt  -> generate dt
        D.FFunc fun -> generate fun
        D.FImp  txt -> generate txt

instance (Render s ConsBlock, Render s Block) => Generator Pragma s where
    generate = \case
        Enable  txt -> "%" `app` "+" `app` fromText txt :: Builder s Tok
        Disable txt -> "%" `app` "-" `app` fromText txt :: Builder s Tok
        Push    txt -> "%" `app` "push" `app` fromText txt :: Builder s Tok
        Pop     txt -> "%" `app` "pop" `app` fromText txt :: Builder s Tok

instance (Render s ConsBlock, Render s Block) => Generator D.Imp s where
    generate = \case
        D.ModImp path _        -> genPath path
        D.DeclImp path targets -> genPath path `appWithoutSpace` ":" `app` (block $ fmap generate targets)  
        where genPath path  = "import" `app` fromText (Text.intercalate "." (fmap toText path)) :: Builder s Tok

instance (Render s ConsBlock, Render s Block) => Generator D.ImpTgt s where
    generate = \case
        D.ImpType nm (Just as) -> (fromText (toText nm) :: Builder s Tok) `app` "as" `app` (fromText (toText as) :: Builder s Tok)
        D.ImpVar nm (Just as)  -> (fromText (toText nm) :: Builder s Tok) `app` "as" `app` (fromText (toText as) :: Builder s Tok)
        D.ImpVar nm Nothing    -> (fromText (toText nm) :: Builder s Tok)
        D.ImpType nm Nothing   -> (fromText (toText nm) :: Builder s Tok)
        D.Wildcard _   -> "*"

instance (Generator v s, Render s ConsBlock, Render s Block) => Generator (Maybe v) s where
    generate = \case
        Nothing -> ""
        Just v  -> generate v

instance Generator Text s where
    generate = convert