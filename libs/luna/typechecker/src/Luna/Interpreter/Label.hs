module Luna.Interpreter.Label where

import Luna.Interpreter.Location.CallPointPath (CallPointPath)

import Prologue

import Data.Construction


data Label = Label { _location :: CallPointPath
                   , _required :: Bool
                   , _dirty    :: Bool
                   , _userNode :: Bool
                   , _checked  :: Bool  -- TODO: to remove
                   } deriving Show


makeLenses ''Label

instance Default Label where
    def = Label def False False False False

instance Monad m => Destroyer m Label where
    destroy _ = return ()
