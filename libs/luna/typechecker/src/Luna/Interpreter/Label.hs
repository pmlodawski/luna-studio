module Luna.Interpreter.Label where

import Luna.Interpreter.Location.CallPointPath (CallPointPath)

import Prologue



data Label = Label { _location :: CallPointPath
                   , _required :: Bool
                   , _dirty    :: Bool
                   , _userNode :: Bool
                   , _checked  :: Bool
                   } deriving Show


makeLenses ''Label

instance Default Label where
    def = Label def False False False False
