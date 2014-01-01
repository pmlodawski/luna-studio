---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Luna.Data.AST.SourcePos where

import Flowbox.Prelude

-- this implementation is limited by hardcoded SourcePos in Parsec
-- appropriate bug is reported
data SourcePos = SourcePos { name   :: String
                           , line   :: Int
                           , column :: Int
                           }
               --deriving (Show)




data SourceRange = SourceRange { begin :: SourcePos
                               , end   :: SourcePos
                               }
                 deriving (Show)


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Show SourcePos where
    show sp = "SourcePos " ++ show(line sp) ++ ":" ++ show(column sp)


--instance Show SourceRange where
--    show sr = "SourceRange " ++ show(begin sr) ++ " - " ++ show(end sr)