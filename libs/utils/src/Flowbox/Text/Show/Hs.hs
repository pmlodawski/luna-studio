---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts#-}

module Flowbox.Text.Show.Hs (
    hsShow
) where

import           Flowbox.Prelude   
import Control.Monad.State
import Control.Applicative

data St = Struct
           | Paren
           | None
           deriving (Show, Eq)


hsShow :: String -> String
hsShow s = out 
    where (out, _) = runState (_hsShow 0 s) [None] 

indent :: Int
indent = 4

newline :: Int -> String
newline i = "\n" ++ replicate (indent*i) ' '

pushState :: MonadState [b] m => b -> m ()
pushState st = do
    s <- get
    put $ st : s

popState :: MonadState [b] m => m ()
popState = do
    (s:sx) <- get
    put $ sx

topState :: MonadState [b] m => m b
topState = do
    (s:sx) <- get
    return s

_hsShow :: (Applicative m, MonadState [St] m) => Int -> String -> m String
_hsShow i s = case s of
    []     -> pure []
    (x:xs) -> if (take 3 s == "{-#") || (take 3 s == "#-}") 
                  then (take 3 s ++) <$> _hsShow i (drop 3 s)
                  else case x of
                      '(' -> do pushState Paren
                                (x :) <$> _hsShow i xs
                      ')' -> do popState
                                (x :) <$> _hsShow i xs
                      '{' -> do pushState Struct
                                ((x : newline (i+1)) ++) <$> _hsShow (i+1) xs
                      ';' -> ((x : newline (i)) ++) <$> _hsShow i xs
                      ',' -> do st <- topState
                                if st == Struct then ((x : newline (i)) ++) <$> _hsShow i xs
                                                else (x:) <$> _hsShow i xs
                      '}' -> do popState
                                (newline (i-1) ++) <$> ((x:) <$> _hsShow (i-1) xs) -- fixme st
                      _   -> (x:) <$> _hsShow i xs
