{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Luna.Syntax.Builder.Cache.Lit () where

-- import Prologue                    hiding (Cons)
-- 
-- import Data.Variant.Patterns       (ANY, MatchSet)
-- import Luna.Syntax.AST.Term
-- import Luna.Syntax.Builder.Cache.Cache
-- 
-- import qualified Luna.Syntax.AST.Layout as Layout
-- import qualified Data.Variant.Patterns  as Rec
-- 
-- 
-- -- Cached utils
-- 
-- matchVariant :: _ => (x -> a) -> MatchSet (Lit v t) a
-- matchVariant f = Rec.matchVariant f
-- 
-- -- Any
-- 
-- matchAny :: (ANY -> a) -> MatchSet (Lit v t) a
-- matchAny = matchVariant
-- {-# NOINLINE matchAny #-}
-- 
-- -- Variants
-- 
-- matchStar :: (Star -> a) -> MatchSet (Lit v t) a
-- matchStar = matchVariant
-- {-# NOINLINE matchStar #-}
-- 
-- matchStr :: (Str -> a) -> MatchSet (Lit v t) a
-- matchStr = matchVariant
-- {-# NOINLINE matchStr #-}
-- 
-- matchNumber :: (Number -> a) -> MatchSet (Lit v t) a
-- matchNumber = matchVariant
-- {-# NOINLINE matchNumber #-}
-- 
-- -- Instances
-- 
-- instance MatchCached ANY    (Lit v t) where matchCached = matchAny    ; {-# INLINABLE matchCached #-}
-- instance MatchCached Star   (Lit v t) where matchCached = matchStar   ; {-# INLINABLE matchCached #-}
-- instance MatchCached Str    (Lit v t) where matchCached = matchStr    ; {-# INLINABLE matchCached #-}
-- instance MatchCached Number (Lit v t) where matchCached = matchNumber ; {-# INLINABLE matchCached #-}
