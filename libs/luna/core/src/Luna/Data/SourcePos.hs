---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Data.SourcePos where

import Flowbox.Prelude

-- this implementation is limited by hardcoded SourcePos in Parsec
-- appropriate bug is reported
data SourcePos = SourcePos { line   :: Int
                           , column :: Int
                           }
               --deriving (Show)




data SourceRange = SourceRange { begin :: Maybe SourcePos
                               , end   :: Maybe SourcePos
                               }
                 deriving (Show)


------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Show SourcePos where
    show sp = "SourcePos (" ++ show(line sp) ++ ", " ++ show(column sp) ++ ")"


instance Monoid SourcePos where
    mempty       = SourcePos 0 0
    mappend _ s2 = s2


instance Monoid SourceRange where
    mempty          = SourceRange mempty mempty
    mappend sr1 sr2 = SourceRange begin' end'
        where begin' = case (begin sr1) of
                       Nothing -> (begin sr2)
                       Just v  -> Just v
              end'   = case (end sr2) of
                       Nothing -> (end sr1)
                       Just v  -> Just v
