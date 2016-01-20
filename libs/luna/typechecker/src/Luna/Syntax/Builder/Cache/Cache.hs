{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}

module Luna.Syntax.Builder.Cache.Cache where

--import Prologue
--import Data.Variant.Patterns (ANY, MatchSet, MatchResolver, resolveMatch)
--import Data.Base             (Base)


--class                                                                           Match rec v   where match :: forall a. (v -> a) -> MatchSet rec a
--instance {-# OVERLAPPABLE #-} (Match' r rec v, MatchResolver rec (Base v) r) => Match rec v   where match = match' (resolveMatch (Proxy :: Proxy rec) (Proxy :: Proxy (Base v))) ; {-# INLINABLE match #-}
--instance {-# OVERLAPPABLE #-} MatchCached ANY rec                            => Match rec ANY where match = matchCached                                                          ; {-# INLINABLE match #-}
--instance {-# OVERLAPPABLE #-}                                                   Match I   v   where match = impossible                                                           ; {-# INLINABLE match #-}

--class                                                       Match' t              rec v where match' :: Proxy (t :: Maybe *) -> (v -> a) -> MatchSet rec a
--instance {-# OVERLAPPABLE #-} (MatchCached v rec, t ~ v) => Match' ('Just (g, t)) rec v where match' _ = matchCached ; {-# INLINABLE match' #-}


--class MatchCached v t where matchCached :: forall a. (v -> a) -> MatchSet t a

