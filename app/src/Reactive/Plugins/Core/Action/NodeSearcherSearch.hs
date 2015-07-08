{-# LANGUAGE OverloadedStrings #-}

module Reactive.Plugins.Core.Action.NodeSearcherSearch where

import Data.Maybe
import Data.List
import Control.Applicative

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

class Nameable a where
    name :: a -> Text

nameToLower :: (Nameable a) => a -> Text
nameToLower t = Text.toLower (name t)

data Submatch = Submatch {start :: Int, len :: Int} deriving (Show, Eq)

data Match a =   ExactMatch a
               | SubstringMatch a [Submatch]
               | AliasMatch a Text (Match a)
          deriving (Show, Eq)

instance Eq t => Ord (Match t) where
  a `compare` b = (score a) `compare` (score b)

data Alias = Alias Text Text deriving (Eq, Show)

findSuggestions :: (Nameable a, Ord (Match a), Eq (Match a)) => [a] -> [Alias] -> Text -> [Match a]
findSuggestions index aliases query =
    reverse $ sort $ (mapMaybe (tryMatch lowQuery) index)
    where
        lowQuery = Text.toLower query
  --   ++ (mapMaybe tryMatchAlias' aliases)
  -- where
  --   tryMatchAlias' (Alias choice to) = fmap (\match -> AliasMatch choice to match) $ tryMatch query choice


score :: Match a -> Double
score match = case match of
    ExactMatch _       -> 1.0
    SubstringMatch _ m -> subseqWeight / (fromIntegral $ (span + length m))
                            where span = (start $ last m) + (len $ last m) - (start $ head m)
    AliasMatch _ _ m   -> aliasWeight * score m
  where
    subseqWeight = 0.9
    aliasWeight  = 0.8


tryMatch :: Nameable a => Text -> a -> Maybe (Match a)
tryMatch  ""    _     = Nothing
tryMatch query choice
    | nameToLower choice == ""   = Nothing
    | otherwise                  = listToMaybe $ catMaybes [isExactMatch, isSubstringMatch]
            where
                            isExactMatch = if (query == (nameToLower choice)) then Just $ ExactMatch choice else Nothing
                            isSubstringMatch = fmap (SubstringMatch choice) $ findSubsequenceOf query (nameToLower choice)

commonPrefixLength :: Text -> Text -> Int
commonPrefixLength "" _  = 0
commonPrefixLength _  "" = 0
commonPrefixLength x y
  | (Text.head x) == (Text.head y)   = 1 + commonPrefixLength (Text.tail x) (Text.tail y)
  | otherwise = 0

shorterMatch :: Maybe [Submatch] -> Maybe [Submatch] -> Maybe [Submatch]
shorterMatch Nothing Nothing   = Nothing
shorterMatch (Just a) Nothing  = Just a
shorterMatch Nothing (Just b)  = Just b
shorterMatch (Just a) (Just b) = if (length a) <= (length b) then Just a else Just b

findSubsequenceOf :: Text -> Text -> Maybe [Submatch]
findSubsequenceOf =
  let
    findSubsequenceOf' :: Int -> Text -> Text -> Maybe [Submatch]
    findSubsequenceOf' _   ""  ""                   = Just []
    findSubsequenceOf' _   _   ""                   = Nothing
    findSubsequenceOf' idx a   b     | prefixLength > 0 = shorterMatch takePrefix skipHead
                                     | otherwise        = skipHead
                               where
                                         prefixLength      = commonPrefixLength a b
                                         dropPrefix        = Text.drop $ fromIntegral prefixLength
                                         skipHead          = findSubsequenceOf' (idx+1) a (Text.tail b)
                                         takePrefix        = fmap ((Submatch idx prefixLength):) $ findSubsequenceOf' (idx+prefixLength) (dropPrefix a) (dropPrefix b)
  in
    findSubsequenceOf' 0

-- main = do
--   putStrLn $ show $ findSuggestions myIndex myAliases "pe"
--   putStrLn $ show $ findSuggestions myIndex myAliases "ple"
--   putStrLn $ show $ findSuggestions myIndex myAliases "fno"