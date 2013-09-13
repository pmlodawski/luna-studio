---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Text.Show.Pretty (
	module Flowbox.Text.Show.Pretty,
	module Text.Show.Pretty
) where

import           Text.Show.Pretty
import           Text.PrettyPrint
import           Flowbox.Generics.Deriving.QShow


ppqShow :: QShow a => a -> String
ppqShow = show . ppqDoc

-- | Try to show a value, prettily. If we do not understand the value, then we
--   just use its standard 'Show' instance.
ppqDoc :: QShow a => a -> Doc
ppqDoc a = case parseValue txt of
            Just v  -> ppValue v
            Nothing -> text txt
  where txt = qshow a