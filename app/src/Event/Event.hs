module Event.Event where

import Data.Monoid
import Data.Typeable
import Data.Dynamic
import Data.Default
import Control.Lens

import Object.Object ( Object )
import Object.Dynamic
import Utils.PrettyPrinter

import qualified Event.Keyboard as Keyboard
import qualified Event.Mouse    as Mouse
import qualified Event.WithObjects as WithObjects


data Event obj = Keyboard Keyboard.Event
               | Mouse    (Mouse.MEvent obj)


makeLenses ''Event



instance Typeable obj => UnpackDynamic (Event Dynamic) (Event obj) where
    unpackDynamic (Mouse (WithObjects.WithObjects ev obj)) = Mouse (WithObjects.WithObjects ev $ unpackDynamic obj)
    unpackDynamic (Keyboard ev)                            = Keyboard ev


-- instance Typeable obj => UnpackDynamic (WithObjects evnt Dynamic) (WithObjects evnt obj) where
--     unpackDynamic = objects %~ unpackDynamic

-- instance (PrettyPrinter evnt, PrettyPrinter obj) => PrettyPrinter (WithObjects evnt obj) where
--     display (WithObjects ev objs) = "wo( " <> display ev <> " " <> display objs <> " )"
