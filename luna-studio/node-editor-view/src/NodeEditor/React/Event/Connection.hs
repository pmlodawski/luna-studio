{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData     #-}

module NodeEditor.React.Event.Connection where

import           Common.Prelude


data ModifiedEnd = Source | Destination
        deriving (Eq, Generic, NFData, Show, Typeable)
