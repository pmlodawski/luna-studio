{-# LANGUAGE OverloadedStrings #-}

module Utils.Searcher where

import Data.Maybe
import Data.List
import Control.Applicative
import qualified Data.Ord

import qualified Data.Text.Lazy as Text
import           Data.Text.Lazy (Text)

import           Data.Char ( isAlphaNum, isUpper )

class Nameable a where
    name :: a -> Text

nameToLower :: (Nameable a) => a -> Text
nameToLower t = Text.toLower (name t)

data Submatch = Submatch { start :: Int
                         , len   :: Int
                         } deriving (Show, Eq)

data Match a = Match Double a [Submatch]
             deriving (Show, Eq)

instance Eq t => Ord (Match t) where
    (Match a _ _) `compare` (Match b _ _) = a `compare` b

data Alias = Alias Text Text deriving (Eq, Show)

findSuggestions :: (Nameable a, Ord (Match a), Eq (Match a)) => [a] -> [Alias] -> Text -> [Match a]
findSuggestions index aliases query =
    reverse $ sort $ (mapMaybe (tryMatch lowQuery) index)
    where
        lowQuery = Text.toLower query
  --   ++ (mapMaybe tryMatchAlias' aliases)
  -- where
  --   tryMatchAlias' (Alias choice to) = fmap (\match -> AliasMatch choice to match) $ tryMatch query choice

tryMatch :: Nameable a => Text -> a -> Maybe (Match a)
tryMatch ""    _                 = Nothing
tryMatch query choice
    | name choice == ""          = Nothing
    | otherwise                  = isSubstringMatch
    where
        isSubstringMatch = fmap (\x -> Match (rank (name choice) query x) choice x) bestMatchVal
        bestMatchVal = bestMatch matches
        bestMatch :: [[Submatch]] -> Maybe [Submatch]
        bestMatch [] = Nothing
        bestMatch m  = Just $ maximumBy (compareMatches $ name choice) m
        matches :: [[Submatch]]
        matches = findSubsequenceOf query (name choice)


commonPrefixLength :: Text -> Text -> Int
commonPrefixLength "" _  = 0
commonPrefixLength _  "" = 0
commonPrefixLength x y
    | (Text.head x) == (Text.head y) = 1 + commonPrefixLength (Text.tail x) (Text.tail y)
    | otherwise                      = 0

findSubsequenceOf :: Text -> Text -> [[Submatch]]
findSubsequenceOf = findSubsequenceOf' 0 where
    findSubsequenceOf' :: Int -> Text -> Text -> [[Submatch]]
    findSubsequenceOf' _   ""  ""                    = [[]]
    findSubsequenceOf' _   _   ""                    = []
    findSubsequenceOf' idx a   b  | prefixLength > 0 = takePrefix ++ skipHead
                                  | otherwise        = skipHead
                                  where
                                       prefixLength  = commonPrefixLength (Text.toLower a) (Text.toLower b)
                                       dropPrefix    = Text.drop . fromIntegral
                                       skipHead      = findSubsequenceOf' (idx + 1) a (Text.tail b)
                                       takePrefix    = concatMap (\l -> fmap ((Submatch idx l):)
                                           $ findSubsequenceOf' (idx + l) (dropPrefix l a) (dropPrefix l b)) [1..prefixLength]

compareMatches :: Text -> [Submatch] -> [Submatch] -> Ordering
compareMatches name a b = (countWordBoundaries name a) `compare` (countWordBoundaries name b)
-- TODO `mappend` more criterions (first start offset, number of matches)

-- Ranking algorithm heavily inspired on Textmate implementation: https://github.com/textmate/textmate/blob/master/Frameworks/text/src/ranker.cc
rank :: Text -> Text -> [Submatch] -> Double
rank choice query match
    | n == capitalsTouched = (denom - 1) / denom + penalty
    | otherwise            = let subtract = substrings * n + (n - capitalsTouched) in (denom - subtract) / denom + penalty
    where
        capitalsTouched = fromIntegral $ countWordBoundaries choice match
        totalCapitals   = fromIntegral $ countTrue $ wordBoundaries choice
        n               = fromIntegral $ Text.length query
        m               = fromIntegral $ Text.length choice
        denom           = n*(n+1) + 1.0
        substrings      = fromIntegral $ length match
        prefixSize      = case match of
            ((Submatch 0 l):xs) -> fromIntegral l
            _ -> fromIntegral 0
        penalty = (m - prefixSize) / m / (2.0*denom) + capitalsTouched / totalCapitals / (4.0*denom) + n / m / (8.0*denom)


countWordBoundaries :: Text -> [Submatch] -> Int
countWordBoundaries t sm = sum $ fmap (countTrue . substring) sm where
     wb = wordBoundaries t
     substring :: Submatch -> [Bool]
     substring (Submatch s l) = take l $ drop s wb

countTrue :: [Bool] -> Int
countTrue list = sum $ map fromEnum list

wordBoundaries :: Text -> [Bool]
wordBoundaries t = reverse out where
                   f :: (Bool, [Bool]) -> Char -> (Bool, [Bool])
                   f (atBow, l) c = (((not $ isAlphaNum c) && c /= '.'), (atBow && isAlphaNum c || isUpper c):l)
                   (_, out) = Text.foldl f (True, []) t
