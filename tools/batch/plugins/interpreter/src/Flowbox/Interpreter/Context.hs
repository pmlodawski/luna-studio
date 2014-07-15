---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
module Flowbox.Interpreter.Context where

import qualified Pipes.Concurrent as Pipes

import Flowbox.Prelude hiding (Context)



data Request = InvalidateCall
             | InvalidateNode
             | InvalidateDef
             | Run
             | WatchPointAdd
             | WatchPointRemove
             | WatchPointList
             deriving (Show)

data Result = Result
            deriving (Show)

data Context = Context { _requestOutput :: Pipes.Output Request
                       , _requestInput  :: Pipes.Input  Request
                       , _resultOutput  :: Pipes.Output Result
                       , _resultInput   :: Pipes.Input  Result
                       }


makeLenses(''Context)


mk :: IO Context
mk = do
    (reqOutput, reqInput) <- Pipes.spawn Pipes.Unbounded
    (resOutput, resInput) <- Pipes.spawn Pipes.Unbounded
    return $ Context reqOutput reqInput resOutput resInput
