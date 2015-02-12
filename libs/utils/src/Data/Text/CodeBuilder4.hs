-----------------------------------------------------------------------------
---- Copyright (C) Flowbox, Inc - All Rights Reserved
---- Unauthorized copying of this file, via any medium is strictly prohibited
---- Proprietary and confidential
---- Flowbox Team <contact@flowbox.io>, 2014
-----------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE UndecidableInstances      #-}

module Data.Text.CodeBuilder4 where

import Prelude ()
import           Flowbox.Prelude hiding (simple)

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Builder as Text
import           Data.Text.Lazy.Builder   (toLazyText, fromLazyText)
--import           Data.Text.Builder.Poly   (ToTextBuilder, toTextBuilder)

import Control.Monad.Identity (runIdentity)
import Control.Monad.State    hiding (mapM)

------------------------------------------------------------------------
---- Data types
------------------------------------------------------------------------

--data CodeType a = Simple  { _code :: a }
--                | Complex { _code :: a }
--                deriving (Show, Eq, Generic, Read, Functor)

--makeLenses ''CodeType

------------------------------------------------------------------------
---- Type classes
------------------------------------------------------------------------

--class CodeBuilder c where
--    simple   :: Text.Builder -> c
--    complex  :: Text.Builder -> c

--class Generator g where
--    generate :: CodeBuilder a => g -> a


------------------------------------------------------------------------
---- Utils
------------------------------------------------------------------------

--simplify :: CodeBuilder a => (CodeType Text.Builder) -> a
--simplify = simplifyWith (\c -> "(" <> c <> ")")

--simplifyWith :: CodeBuilder a => (Text.Builder -> Text.Builder) -> (CodeType Text.Builder) -> a
--simplifyWith f = \case
--    Simple  c -> simple c
--    Complex c -> simple $ f c

--simple' :: (ToTextBuilder a, CodeBuilder c) => a -> c
--simple'  = simple.toTextBuilder

--complex' :: (ToTextBuilder a, CodeBuilder c) => a -> c
--complex' = complex.toTextBuilder

--genmap :: (Generator g, CodeBuilder a) => [g] -> [a]
--genmap = map generate

--sgenmap :: (Generator g, CodeBuilder a) => [g] -> [a]
--sgenmap = map sgenerate

--sgenerate :: (Generator g, CodeBuilder a) => g -> a
--sgenerate = simplify.generate


------------------------------------------------------------------------
---- Instances
------------------------------------------------------------------------

---- basic

--instance Applicative CodeType where
--    pure = Simple
--    l <*> r = case l of
--        Simple f -> case r of
--            Simple  v -> Simple  $ f v
--            Complex v -> Complex $ f v
--        Complex f -> Complex $ f (r ^. code)


---- CodeBuilder

--instance CodeBuilder (CodeType Text.Builder) where
--    simple  = Simple
--    complex = Complex

--instance CodeBuilder Text.Builder where
--    simple   = id
--    complex  = id


---- Convertible

----instance Convertible Text Text.Builder where
----    convert = fromLazyText

----instance Convertible String Text.Builder where
----    convert = fromString

----instance Convertible Text.Builder Text.Builder where
----    convert = id

----instance Convertible a b => Convertible [a] [b] where
----    convert = fmap convert



--simple <> Seq 
--term s


--data CodeType a = Simple  { _code :: a }
--                | Complex { _code :: a }
--                deriving (Show, Eq, Generic, Read, Functor)


--data Layout a = Single a 
--              | Seq a 


--data Code = Txt     Text.Builder
--          | Simple  Code
--          | Complex Code
--          | Seq   [Code]
--          | Expr    [Code]
--          deriving (Show, Eq, Generic)


type Prec = Int
type Name = Text.Builder 

--data Fixity = Prefix
--            | Postfix
--            | Infix Assoc
--            deriving (Show, Eq, Generic)

data Assoc = ALeft
           | ARight
           deriving (Show, Eq, Generic)

data Code = Tok  Name
          | SBox Code
          | Seq  [Code]
          | App  Op
          deriving (Show, Eq, Generic)


data Fixity = Prefix        Code
            | Postfix       Code
            | Infix   Assoc Code Code
            deriving (Show, Eq, Generic)

data Op = Op Prec Code Fixity
        deriving (Show, Eq, Generic)


instance IsString Code where
    fromString = Tok . fromString


app n c = App $ Op 10 n $ Prefix c
apps = foldl app
--app2 n c = App $ Op 11 n $ Prefix c


test = app "foo" $ app "bar" "x"   -- foo (bar x)
test2 = app (app "foo" "bar") "x"  -- (foo bar) x

test3 = h_func "foo" ["a", "b", "c"] "5"

h_func name args body = apps (apps name args) ["=", Seq [test2, test2]]
--test = app (Tok "foo") $ app (Tok "bar") (Tok "x")

data HSCompact = HSCompact deriving (Show)

data IndentState = IndentState { _indent :: Int }
                 deriving (Show)

makeLenses ''IndentState

class Render style m where
    render :: (Monad m, Applicative m) => style -> Code -> m Text.Builder

between l r t = l <> t <> r

instance MonadState IndentState m => Render HSCompact m where
    render style = \case
        Tok n -> return n
        App (Op prec name f) -> case f of
            Prefix code -> (\n c -> n <> conv c) <$> render style name <*> render style code
                where conv = if prec >= getPrec code then between "(" ")"
                                                     else (" " <>)
        Seq cs -> indented $ do
            ind <- getIndentTxt
            let indent = "\r\n" <> ind
            ("do " <>) . (indent <>) . mjoin indent <$> mapM (render style) cs
        SBox c -> render style c
        where getPrec = \case
                  App (Op p _ _) -> p
                  _     -> 100




getIndentTxt = (\i -> fromString $ replicate (4*i) ' ') <$> getIndent
getIndent = view indent <$> get
setIndent ind = do
    s <- get
    put $ s & indent .~ ind

indented p = do
    s <- get
    put $ s & indent %~ (+1)
    ret <- p
    put s
    return ret

instance Default IndentState where
    def = IndentState def

pText = putStrLn . Text.unpack . toLazyText 

main = do
    --print test
    --print $ runIdentity $ render HSCompact test
    --print $ runIdentity $ render HSCompact test2
    pText $ flip evalState (def :: IndentState) $ render HSCompact test3
    return ()
