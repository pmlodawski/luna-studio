---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}

module Flowbox.Graphics.Image.Color.Helpers where

import           Data.Array.Accelerate as A

import           Flowbox.Graphics.Image         (Image)
--import qualified Flowbox.Graphics.Image         as Image
import qualified Flowbox.Graphics.Image         as Image
import           Flowbox.Graphics.Image.Channel (ChannelAcc)
import qualified Flowbox.Graphics.Image.Channel as Channel
import           Flowbox.Graphics.Utils         as U
import           Flowbox.Prelude



threeWayConvert :: (Shape ix, Elt a, Elt b, Image imgA (ChannelAcc ix a), Image imgB (ChannelAcc ix b))
    => (t -> imgB (ChannelAcc ix b)) -> (Channel.Name, Channel.Name, Channel.Name)
    -> (Exp (a, a, a) -> Exp (b, b, b)) -> (Channel.Name, Channel.Name, Channel.Name)
    -> imgA (ChannelAcc ix a) -> t
    -> Either Image.Error (imgB (ChannelAcc ix b))
threeWayConvert convertTo (nameOutA, nameOutB, nameOutC) converter (nameInA, nameInB, nameInC) img chans = do
    chanInA <- Image.get nameInA img
    chanInB <- Image.get nameInB img
    chanInC <- Image.get nameInC img
    let chansIn = A.zip3 (Channel.accMatrix chanInA) (Channel.accMatrix chanInB) (Channel.accMatrix chanInC)
        chansOut = A.map converter chansIn
        chanOutA = Channel.Acc $ A.map U.fstTrio chansOut
        chanOutB = Channel.Acc $ A.map U.sndTrio chansOut
        chanOutC = Channel.Acc $ A.map U.trdTrio chansOut
    return $ Image.insert nameOutA chanOutA
           $ Image.insert nameOutB chanOutB
           $ Image.insert nameOutC chanOutC
           $ convertTo chans

convertFromFour :: (A.Shape ix, A.Elt a, A.Elt b, Image imgA (ChannelAcc ix a), Image imgB (ChannelAcc ix b))
    => (Channel.Name, Channel.Name, Channel.Name) -> (Channel.Name, Channel.Name, Channel.Name, Channel.Name)
    -> (A.Exp (a, a, a, a) -> A.Exp (b, b, b))
    -> (t -> imgB (ChannelAcc ix b)) -> imgA (ChannelAcc ix a) -> t
    -> Either Image.Error (imgB (ChannelAcc ix b))
convertFromFour (nameOutA, nameOutB, nameOutC) (nameInA, nameInB, nameInC, nameInD) converter convertTo img chans = do
    inA <- Image.get nameInA img
    inB <- Image.get nameInB img
    inC <- Image.get nameInC img
    inD <- Image.get nameInD img
    let chansIn = A.zip4 (Channel.accMatrix inA) (Channel.accMatrix inB) (Channel.accMatrix inC) (Channel.accMatrix inD)
        chansOut = A.map converter chansIn
        chanOutA = Channel.Acc $ A.map U.fstTrio chansOut
        chanOutB = Channel.Acc $ A.map U.sndTrio chansOut
        chanOutC = Channel.Acc $ A.map U.trdTrio chansOut
    return $ Image.insert nameOutA chanOutA
           $ Image.insert nameOutB chanOutB
           $ Image.insert nameOutC chanOutC
           $ convertTo chans
