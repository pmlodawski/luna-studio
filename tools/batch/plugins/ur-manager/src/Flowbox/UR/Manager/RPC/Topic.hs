---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Flowbox.UR.Manager.RPC.Topic where

urmClearStackRequest        = "urm.clearstack.request"
urmDescriptionsCleared      = "urm.descriptions.cleared"
urmPingRequest              = "urm.ping.request"
urmRedoDescriptionRemoved   = "urm.redo.descriptions.removed"
urmRedoDescriptionRequest   = "urm.redo.descriptions.request"
urmRedoRequest              = "urm.redo.request"
urmRegisterMultipleRequest  = "urm.registermultiple.request"
urmRegisterRequest          = "urm.register.request"
urmTransactionBeginRequest  = "urm.transaction.begin.request"
urmTransactionCommitRequest = "urm.transaction.commit.request"
urmUndoDescriptionAdded     = "urm.undo.descriptions.added"
urmUndoDescriptionRemoved   = "urm.undo.descriptions.removed"
urmUndoDescriptionRequest   = "urm.undo.descriptions.request"
urmUndoRequest              = "urm.undo.request"
