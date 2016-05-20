module Reactive.Commands.UUID where

import           Utils.PreludePlus
import           Reactive.Commands.Command     (Command, execCommand, performIO)
import qualified Reactive.Commands.UIRegistry  as UICmd
import           Reactive.State.Global         (State, nextRandom)

import Data.UUID.Types
import Data.UUID.Types.Internal ( buildFromBytes )

import Data.ByteString ( unpack )

getUUID :: Command State UUID
getUUID = do
  [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, ba, bb, bc, bd, be, bf] <- mapM (const nextRandom) [1..16]
  return $ buildFromBytes 4 b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf
