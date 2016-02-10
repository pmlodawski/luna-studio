module Luna.Compilation.Pass.Dirty.Data.Label where

import           Luna.Compilation.Pass.Dirty.Data.CallPointPath (CallPointPath)

import           Data.Construction
import           Luna.Syntax.Model.Layer
import           Prologue



data DirtyVal = DirtyVal
        { _location :: CallPointPath
        , _required :: Bool
        , _dirty    :: Bool
        , _userNode :: Bool
        } deriving Show


makeLenses ''DirtyVal

instance Default DirtyVal where
    def = DirtyVal def False False False


data Dirty = Dirty deriving (Show, Eq, Ord)
type instance LayerData layout Dirty t = DirtyVal
instance Monad m => Creator    m (Layer layout Dirty a) where create = return $ Layer def
instance Monad m => Destructor m (Layer layout Dirty t) where destruct _ = return ()
