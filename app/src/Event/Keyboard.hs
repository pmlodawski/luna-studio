module Event.Keyboard where

import           Utils.PreludePlus


data Type = Press | Down | Up deriving (Eq, Show)

data Event = Event { _tpe  :: Type
                   , _char :: Char
                   } deriving (Eq, Show)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Event t c) = show t <> " " <> show c



-- others

data KeyMods = KeyMods { _shift :: Bool
                       , _ctrl  :: Bool
                       , _alt   :: Bool
                       , _meta  :: Bool
                       } deriving (Eq, Show)

makeLenses ''KeyMods

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
