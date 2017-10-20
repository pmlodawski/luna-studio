{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.React.Event.InputField where

import           Common.Prelude
import           NodeEditor.React.Model.InputField (InputFieldId)


data Event = ActivateInputField InputFieldId
            deriving (Show, Generic, NFData, Typeable)
