module Event.Keyboard where

import Utils.PreludePlus
import Data.Aeson (ToJSON)

data Type = Press | Down | Up deriving (Eq, Show, Generic)

data KeyMods = KeyMods { _shift :: Bool
                       , _ctrl  :: Bool
                       , _alt   :: Bool
                       , _meta  :: Bool
                       } deriving (Eq, Show, Generic)

data Event = Event { _tpe     :: Type
                   , _char    :: Char
                   , _keyMods :: KeyMods
                   } deriving (Eq, Show, Generic)

makeLenses ''Event
makeLenses ''KeyMods

instance ToJSON KeyMods
instance ToJSON Type
instance ToJSON Event

instance PrettyPrinter Event where
    display (Event t c m) = show t <> " " <> show c <> " " <> show m

instance Default KeyMods where
    def = KeyMods False False False False

getString :: String -> Bool -> Maybe String
getString str cond = if cond then Just str else Nothing

instance PrettyPrinter KeyMods where
    display keyMods = intercalate " " $ catMaybes [ getString "shift" $ keyMods ^. shift
                                                  , getString "ctrl"  $ keyMods ^. ctrl
                                                  , getString "alt"   $ keyMods ^. alt
                                                  , getString "meta"  $ keyMods ^. meta
                                                  ]

