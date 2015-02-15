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
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE OverlappingInstances      #-}

module Data.Text.CodeBuilder5 where

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



newtype RenderStyleT s m a = RenderStyleT { unRenderStyleT :: StateT s m a }
                         deriving (Monad, MonadIO, MonadPlus, Applicative, Alternative, Functor)

class (Monad m, Applicative m) => MonadRenderStyle s m | m -> s where
    getStyle :: m s
    putStyle :: s -> m ()

runRenderStyleT :: Monad m => RenderStyleT s m a -> s -> m a 
runRenderStyleT = evalStateT . unRenderStyleT

-- == Instances ==

instance MonadState s m => MonadState s (RenderStyleT s m) where
    get = RenderStyleT . lift $ get
    put = RenderStyleT . lift . put

instance (Monad m, Functor m) => MonadRenderStyle s (RenderStyleT s m) where
    getStyle = RenderStyleT $ get
    putStyle = RenderStyleT . put

instance (MonadTrans t, MonadRenderStyle s m, Monad (t m), Applicative (t m)) => MonadRenderStyle s (t m) where
    getStyle = lift getStyle
    putStyle = lift . putStyle

------------------------------------------------------------------------
------------------------------------------------------------------------



data IndentState = IndentState { _indent :: Int }
                 deriving (Show)

makeLenses ''IndentState


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

instance FromText Code where
    fromText = Tok . fromLazyText


pfxApp p a b = App $ Op p a $ Prefix b
pfxApps p = foldl (pfxApp p)

app n c = pfxApp 11 (spaced2 n) c

apps = foldl app

--macroApps base args = SBox $ pfxApps 11 base ("(" : intersperse "," args ++ [")"])
--tuple items = SBox $ pfxApps 11 "(" (intersperse "," items ++ [")"])

spaced2 n = App $ Op 10 n $ Prefix " "



--test = app "foo" $ app "bar" "x"   -- foo (bar x)
--test2 = app (app "foo" "bar") "x"  -- (foo bar) x
--test3 = macroApps (app "foo" "bar") ["x", "y", "z"]  -- (foo bar) x

--doBlock = app "do" . CB.Seq

--doBlock mterms = fmap (app "do") . indented $ do
--    terms <- mterms
--    ind   <- getIndentTxt
--    style <- getStyle
--    let indent = "\n" <> ind :: Text
--        terms' = fmap (pfxApp 1 (fromText indent)) terms
--    return $ SBox $ pfxApps 0 "" terms'

doBlock mterms = indented $ do -- fmap (app "do") . 
    terms <- mterms
    ind   <- getIndentTxt
    style <- getStyle
    let indent = "\n" <> ind :: Text
    renderBlock style terms
    --terms' <- renderBlock style terms -- fmap (pfxApp 1 (fromText indent)) terms
    --return $ SBox $ pfxApps 0 "" terms'


class BlockRenderer style m where
    renderBlock :: (Monad m, Applicative m) => style -> [Code] -> m Code

instance MonadState IndentState m => BlockRenderer HSIndent m where
    renderBlock _ terms = do 
        ind   <- getIndentTxt
        let indent    = fromText $ "\n" <> ind
            termsInd  = fmap (pfxApp 1 indent) terms
            (mod, ts) = if length terms <= 1 then (id      , terms)
                                             else (app "do", termsInd)
        return $ mod . SBox $ pfxApps 0 "" ts

between l r a = pfxApps 99 l [a,r]
betweenL l r a = pfxApps 99 l $ a ++ [r]

instance BlockRenderer HSCompact m where
    renderBlock _ terms = do 
        let terms' = fmap (SBox . flip (pfxApp 1) ";") terms
        return $ app "do" $ betweenL "{" "}" terms'

--dataDeclBlock mterms = indented $ do
--    terms <- mterms
--    ind   <- getIndentTxt
--    let indent = "\n" <> ind <> "| "
--        terms' = fmap (pfxApp 1 (fromText indent)) terms
--    return $ SBox $ pfxApps 0 "" terms'




    --(indent <>) . mjoin indent <$> mapM (render style) cs

--doBlock = indented $ do
--    ind <- getIndentTxt
--    let indent = "\n" <> ind :: Text
--    return $ (\terms -> SBox $ pfxApps 0 "" (fmap (pfxApp 1 (fromText indent)) terms) )

test :: (Applicative m, MonadState IndentState m, MonadRenderStyle s m, BlockRenderer s m) => m Code
test = doBlock $ pure ["x", "y", "z"]
test2 = doBlock $ pure ["x"]

--runMe :: StateT IndentState (RenderStyleT HSCompact Identity) Code -> Name
runMeI = runIdentity . flip runRenderStyleT HSIndent . flip evalStateT (def :: IndentState) . render
runMeC = runIdentity . flip runRenderStyleT HSCompact . flip evalStateT (def :: IndentState) . render


main = do
    pText $ runMeC test
    pText $ runMeI test
    pText $ runMeI test2

    return ()

----test3 = h_func "foo" ["a", "b", "c"] "5"

--h_func name args body = apps (apps name args) ["=", Seq [test, test2]]
----test = app (Tok "foo") $ app (Tok "bar") (Tok "x")

data HSCompact = HSCompact deriving (Show)
data HSIndent  = HSIndent  deriving (Show)

render = fmap renderPure

renderPure = \case
    Tok n -> n
    App (Op prec name f) -> case f of
        Prefix code -> renderPure name <> conv (renderPure code)
            where conv = if prec >= getPrec code then parensed
                                                 else id
    SBox c -> renderPure c
    where getPrec = \case
              App (Op p _ _) -> p
              _     -> 100

--class Render style m where
--    render :: (Monad m, Applicative m) => style -> m Code -> m Text.Builder

betweenStr l r t = l <> t <> r
parensed = betweenStr "(" ")"
--spaced = (" " <>)

--instance MonadState IndentState m => Render HSCompact m where
--    render style = \case
--        Tok n -> return n
--        App (Op prec name f) -> case f of
--            Prefix code -> (\n c -> n <> conv c) <$> render style name <*> render style code
--                where conv = if prec >= getPrec code then parensed
--                                                     else id
--        Seq cs -> indented $ do
--            ind <- getIndentTxt
--            let indent = "\r\n" <> ind
--            (indent <>) . mjoin indent <$> mapM (render style) cs
--        SBox c -> render style c
--        where getPrec = \case
--                  App (Op p _ _) -> p
--                  _     -> 100





getIndentTxt = (\i -> fromString $ replicate (4*i) ' ') <$> getIndent
getIndent = view indent <$> get
--setIndent ind = do
--    s <- get
--    put $ s & indent .~ ind

indented p = do
    s <- get
    put $ s & indent %~ (+1)
    ret <- p
    put s
    return ret

instance Default IndentState where
    def = IndentState 0

pText = putStrLn . Text.unpack . toLazyText 


--main = do
--    --print test
--    --print $ runIdentity $ render HSCompact test
--    --print $ runIdentity $ render HSCompact test2
--    pText $ flip evalState (def :: IndentState) $ render HSCompact test3
--    return ()
