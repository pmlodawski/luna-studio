module Main where

import Control.Monad.State
import Control.Monad.IO.Class


data Mouse = Mouse Int

class HasMouse m where
    getMouse :: m Mouse

class ContainsMouse a where
    takeMouse :: a -> Mouse

instance ContainsMouse Mouse where takeMouse = id
instance ContainsMouse a => ContainsMouse (KeyMods a) where takeMouse (KeyMods a _) = takeMouse a

instance HasMouse (StateT Mouse m) where
    getMouse = get

instance HasMouse (StateT (KeyMods Mouse) m) where
    getMouse = takeMouse <$> get

-- instance HasMouse m => HasMouse (StateT a m) where
--     getMouse = lift getMouse


foo = do
    m <- getMouse
    liftIO $ print m

data Mods = Mods
data KeyMods a = a Mods

main = do
    runStateT foo (Mouse 1)
    runStateT foo (KeyMods (Mouse 1) Mods)

    print $ takeMouse (KeyMods (Mouse 1) Mods)
    print "end"