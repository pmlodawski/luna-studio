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
import Data.List (intersperse)

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


pfxApp p a b = App $ Op p a $ Prefix b
pfxApps p = foldl (pfxApp p)

app n c = pfxApp 11 (spaced2 n) c

apps = foldl app

macroApps base args = SBox $ pfxApps 11 base ("(" : intersperse "," args ++ [")"])
tuple items = SBox $ pfxApps 11 "(" (intersperse "," items ++ [")"])

spaced2 n = App $ Op 10 n $ Prefix " "



test = app "foo" $ app "bar" "x"   -- foo (bar x)
test2 = app (app "foo" "bar") "x"  -- (foo bar) x
test3 = macroApps (app "foo" "bar") ["x", "y", "z"]  -- (foo bar) x

--test3 = h_func "foo" ["a", "b", "c"] "5"

h_func name args body = apps (apps name args) ["=", Seq [test, test2]]
--test = app (Tok "foo") $ app (Tok "bar") (Tok "x")

data HSCompact = HSCompact deriving (Show)

data IndentState = IndentState { _indent :: Int }
                 deriving (Show)

makeLenses ''IndentState

class Render style m where
    render :: (Monad m, Applicative m) => style -> Code -> m Text.Builder

between l r t = l <> t <> r
parensed = between "(" ")"
spaced = (" " <>)

instance MonadState IndentState m => Render HSCompact m where
    render style = \case
        Tok n -> return n
        App (Op prec name f) -> case f of
            Prefix code -> (\n c -> n <> conv c) <$> render style name <*> render style code
                where conv = if prec >= getPrec code then parensed
                                                     else id
        Seq cs -> indented $ do
            ind <- getIndentTxt
            let indent = "\r\n" <> ind
            (indent <>) . mjoin indent <$> mapM (render style) cs
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

runMe = flip evalState (def :: IndentState) . render HSCompact

main = do
    --print test
    --print $ runIdentity $ render HSCompact test
    --print $ runIdentity $ render HSCompact test2
    pText $ flip evalState (def :: IndentState) $ render HSCompact test3
    return ()
