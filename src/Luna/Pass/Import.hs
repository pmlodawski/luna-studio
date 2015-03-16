{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Luna.Pass.Import where

import           Luna.Syntax.Label  (Label(Label), _element)
import           Luna.Syntax.Module (_body, Module(Module))
import qualified Luna.Syntax.Decl   as Dec
import           Luna.Syntax.Name   (TName(TName))
import           Luna.Syntax.Unit   (Unit(Unit))
import           Flowbox.Prelude	

getFromUnit :: Unit a -> a
getFromUnit (Unit a) = a

getFromLabel :: Label l a -> a
getFromLabel (Label l a) = a 

filterImports :: Label l (Dec.Decl a e) -> Bool
filterImports (Label _  (Dec.Imp _))   = True
filterImports _ = False

unpackImport :: Dec.Decl a e -> Dec.Imp
unpackImport (Dec.Imp x) = x

getImportList :: Unit ( Label l0 (Module a0 e0)) -> [Dec.Imp]
getImportList = fmap (unpackImport . _element) . filter filterImports . _body . getFromLabel . getFromUnit


