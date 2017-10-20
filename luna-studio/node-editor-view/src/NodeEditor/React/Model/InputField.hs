{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}
module NodeEditor.React.Model.InputField where

import           Common.Prelude
import           Data.Convert            (Convertible (convert))
import qualified Data.Text               as Text
import           JS.Key                  (toKey)
import           LunaStudio.Data.PortRef (InPortRef)


data InputFieldMode = Inactive | Multiline | Single deriving (Eq, Generic, NFData, Show)

data InputFieldId = PortControlField InPortRef deriving (Eq, Generic, NFData, Show)

data InputField = InputField { _content  :: Text
                             , _mode     :: InputFieldMode
                             } deriving (Generic, Eq, NFData, Show)

type Range = (Int, Int)

data ActiveInputField = ActiveInputField { _fieldContent     :: Text
                                         , _cursorsPositions :: [Int]
                                         , _selection        :: [Range]
                                         }  deriving (Generic, Eq, NFData, Show)

makeLenses ''InputField
makeLenses ''ActiveInputField
makePrisms ''InputFieldMode
makePrisms ''InputFieldId

instance Default InputFieldMode where def = Inactive
instance Default InputField     where def = mkInputField def

instance Convertible InputField ActiveInputField where
    convert f = let cursorPos = Text.length $ f ^. content in ActiveInputField (f ^. content) [cursorPos] [(cursorPos, cursorPos)]

mkInputField :: Text -> InputField
mkInputField t = InputField t def

key :: InputFieldId -> InputField -> JSString
key fid f = toKey $ show fid <> "-" <> show (f ^. mode)
