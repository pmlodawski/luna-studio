---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.Luna.Parser.Utils where

import Prelude hiding(lex)
import Data.Char hiding (Space)
import           Text.ParserCombinators.UU hiding(parse, pMany, (<??>))
import qualified Text.ParserCombinators.UU.Utils as Utils
import Text.ParserCombinators.UU.BasicInstances hiding (Parser)
--import qualified Data.ListLike as LL
--import Text.ParserCombinators.UU.Idioms
--import Text.ParserCombinators.UU.Interleaved

import Debug.Trace

pSyms []       = pReturn []
pSyms (x : xs) = (:) <$> pSym x <*> pSyms xs

pSymOf syms    = pChoice $ map pSym syms

pChoice   = foldr (<|>) pFail
--pMany1 p  = (:) <$> p <*> pMany p
pElem els = pChoice $ map pSym els
pLower    = (pSatisfy isLower) (Insertion ("lower unicode letter") t 5) where t = 'a'
pUpper    = (pSatisfy isUpper) (Insertion ("upper unicode letter") t 5) where t = 'A'
pAlpha    = (pSatisfy isAlpha) (Insertion ("lower unicode letter") t 5) where t = 'a'

pNoneOf sysm = pSatisfy (`notElem` sysm) (Insertion (show t) t 5) where t = '_'

infixl 4  <??>
p <??> q = p <**> (q <|> pure id)

pl <*? pr = pl <??> (id <$ pr)

pl ?*> pr = (pl *> pr) <<|> pr

pl *?> pr = (pl *> (pr <<|> empty))

pl <?*> pr = (pl <*> pr) <<|> pr

--pSpaces :: Parser String
pSpaces = pMunch (`elem` " \t") <?> "Whitespace"

--lexeme :: ParserTrafo a a
lexeme p = p <* pSpaces




pSepBy  sep p = (pSepBy1 sep p) `opt` []
pSepBy1 sep p = (:) <$> p <*> pMany (sep *> p)
pSepBy2 sep p = (:) <$> p <*> pMany1 (sep *> p)

pEOL       = pChoice [pSyms "\n", pSyms "\r", pSyms "\r\n"]

liftList p = (:[]) <$> p

pTrace p   = (\x->trace("pTrace: " ++ show x)x) <$> p

pSpaces1 = (:) <$> pSymOf " \r\n\t" <*> pSpaces


pMany p  = pList_ng p
pMany1 p = (:) <$> p <*> pMany p


pChainL op p = applyAll <$> p <*> pMany (flip <$> op  <*> p)
pChainProcL proc op p = proc <$> (applyAll <$> p <*> pMany (flip <$> op <*> (proc <$> p)))

applyAll x (f:fs) = applyAll (f x) fs
applyAll x []     = x


pSpaced = pPacked pSpaces pSpaces