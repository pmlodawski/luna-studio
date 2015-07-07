module Event.Keyboard where

import Data.Monoid
import Data.Typeable
import Data.Default
import Data.Maybe          ( catMaybes )
import Data.List           ( intercalate )
import Control.Lens
import Utils.PrettyPrinter

data Type = Press | Down | Up deriving (Eq, Show, Typeable)

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

data Event = Event { _tpe :: Type, _char :: Char } deriving (Eq, Show, Typeable)

makeLenses ''Event

instance PrettyPrinter Event where
    display (Event t c) = show t <> " " <> show c

newEvent :: Type -> Char -> Event
newEvent t ch = Event t ch
