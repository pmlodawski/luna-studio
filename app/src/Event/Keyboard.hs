module Event.Keyboard where

import Data.Monoid
import Data.Typeable
import Data.Default
import Data.Maybe          ( catMaybes )
import Data.List           ( intercalate )
import Control.Lens
import Utils.PrettyPrinter

data KeyMods = KeyMods { _shift :: Bool
                       , _ctrl  :: Bool
                       , _alt   :: Bool
                       , _meta  :: Bool
                       } deriving (Eq, Show, Typeable)

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

data Event = Event { _char :: Char } deriving (Eq, Show, Typeable)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Event c) = show c

newEvent :: Char -> Event
newEvent ch = Event ch
