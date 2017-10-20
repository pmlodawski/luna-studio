{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.Event.Atom where

import           Common.Analytics                  (IsTrackedEvent)
import           Common.Prelude
import           Data.Aeson                        (FromJSON, ToJSON)
import           NodeEditor.React.Model.InputField (Range)


data Event = SetFile { path :: FilePath }
           | UnsetFile
           | InputFieldUpdate { content :: Text, cursors :: [Int], selection :: [Range] }
           deriving (Eq, FromJSON, Generic, NFData, Show, ToJSON, Typeable)

instance IsTrackedEvent Event
