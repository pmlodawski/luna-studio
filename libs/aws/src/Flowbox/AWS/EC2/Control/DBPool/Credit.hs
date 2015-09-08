---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.AWS.EC2.Control.DBPool.Credit where

import           Flowbox.AWS.User.User (User)
import qualified Flowbox.AWS.User.User as User
import           Flowbox.Prelude



type Error = String


charge :: User -> Int -> Either Error User
charge user cost = if cost <= user ^. User.credit
    then Right $ user & User.credit %~ flip (-) cost
    else Left "Insufficient credit to perform operation"

