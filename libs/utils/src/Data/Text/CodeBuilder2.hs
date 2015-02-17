---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE OverlappingInstances      #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Text.CodeBuilder2 where

import Prelude ()
import           Flowbox.Prelude hiding (simple)

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.Builder as Text
import           Data.Text.Lazy.Builder   (toLazyText, fromLazyText)
import           Data.Text.Builder.Poly   (ToTextBuilder, toTextBuilder)
import           Control.Monad.State      hiding (mapM)

import Control.Monad.Identity hiding (mapM)

----------------------------------------------------------------------
-- Data types
----------------------------------------------------------------------

data Block a = Block { _terms :: [a] }
             deriving (Show)

data CodeType a = Simple  { _code :: a }
                | Complex { _code :: a }
                deriving (Show, Eq, Generic, Read, Functor)

makeLenses ''CodeType

----------------------------------------------------------------------
-- Type classes
----------------------------------------------------------------------

class CodeBuilder c where
    simple   :: Text.Builder -> c
    complex  :: Text.Builder -> c

class Generator g where
    generate :: CodeBuilder a => g -> a


----------------------------------------------------------------------
-- Utils
----------------------------------------------------------------------

simplify :: CodeBuilder a => (CodeType Text.Builder) -> a
simplify = simplifyWith (\c -> "(" <> c <> ")")

simplifyWith :: CodeBuilder a => (Text.Builder -> Text.Builder) -> (CodeType Text.Builder) -> a
simplifyWith f = \case
    Simple  c -> simple c
    Complex c -> simple $ f c

simple' :: (ToTextBuilder a, CodeBuilder c) => a -> c
simple'  = simple.toTextBuilder

complex' :: (ToTextBuilder a, CodeBuilder c) => a -> c
complex' = complex.toTextBuilder

genmap :: (Generator g, CodeBuilder a) => [g] -> [a]
genmap = map generate

sgenmap :: (Generator g, CodeBuilder a) => [g] -> [a]
sgenmap = map sgenerate

sgenerate :: (Generator g, CodeBuilder a) => g -> a
sgenerate = simplify.generate


----------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------

data IndentState = IndentState { _indent :: Int } deriving (Show)

makeLenses ''IndentState

-- basic

instance Applicative CodeType where
    pure = Simple
    l <*> r = case l of
        Simple f -> case r of
            Simple  v -> Simple  $ f v
            Complex v -> Complex $ f v
        Complex f -> Complex $ f (r ^. code)


-- CodeBuilder

instance CodeBuilder (CodeType Text.Builder) where
    simple  = Simple
    complex = Complex

instance CodeBuilder Text.Builder where
    simple   = id
    complex  = id

data Term a = Term [a] deriving (Show)

test = Complex $ Block [ Simple ("match1" :: Text) , Simple "match2" ]

test2 r = [render r $ Simple ("match1" :: Text), render r $ Simple ("match2" :: Text)]


class Renderer r b m where
    render :: (Monad m, Applicative m) => r -> b -> m Text.Builder

a </> b = a <> "\r\n" <> b
a <+> b = a <> " " <> b

foldlDef f d = \case
    []     -> d
    (x:xs) -> foldl f x xs

between l r t = l <> t <> r

instance Renderer HSCompact a m => Renderer HSCompact (Block a) m where
    render r (Block terms) = braced . foldl (<>) "" <$> mapM renderTerm terms where
        renderTerm t = (<> ";") <$> render r t 
        braced       = between "{" "}"

instance (Renderer HSIndent a m, MonadState IndentState m) => Renderer HSIndent (Block a) m where
    render r (Block terms) = foldl (</>) "" <$> indented (mapM renderTerm terms) where
        renderTerm t = do
            ind <- getIndent
            ((fromString $ replicate ind ' ') <>) <$> render r t 


instance Renderer r a m => Renderer r (Term a) m where
    render r (Term ts) = foldlDef (<+>) "" <$> (mapM (render r) ts)

instance Renderer r a m => Renderer r (CodeType a) m where
    render r = \case
        Simple  a -> render r a
        Complex a -> between "(" ")" <$> render r a

instance Renderer r Text m where
    render _ = return . fromLazyText

data HSCompact = HSCompact deriving (Show)
data HSIndent  = HSIndent  deriving (Show)


getIndent = view indent <$> get
indented p = do
    s <- get
    put $ s & indent %~ (+1)
    r <- p
    put s
    return r

instance Default IndentState where
    def = IndentState 0

main = do
    putStrLn $ Text.unpack . Text.toLazyText $ flip evalState (def :: IndentState) $ render HSIndent test


-- Convertible

--instance Convertible Text Text.Builder where
--    convert = fromLazyText

--instance Convertible String Text.Builder where
--    convert = fromString

--instance Convertible Text.Builder Text.Builder where
--    convert = id

--instance Convertible a b => Convertible [a] [b] where
--    convert = fmap convert

