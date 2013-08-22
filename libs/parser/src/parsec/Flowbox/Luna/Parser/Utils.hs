---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Parser.Utils where

import Text.Parsec

checkIf f msg p = do
	obj <- p
	if (f obj)
		then unexpected (msg ++ show obj)
		else return obj

pl <*$> pr = do 
	n <- pr
	pl n