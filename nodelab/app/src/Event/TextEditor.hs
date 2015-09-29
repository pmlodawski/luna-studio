module Event.TextEditor where


import Utils.PreludePlus
import Object.Object
import Object.UITypes

data Event = CodeModified { _code :: Text } deriving (Show, Typeable)

makeLenses ''Event

instance PrettyPrinter Event where
    display (CodeModified m) = show m
