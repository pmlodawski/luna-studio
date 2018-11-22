{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}

module JS.Mount
  ( isPrefixed
  , mountPoint
  , prefix
  ) where

import qualified Data.JSString       as JSString
import qualified Data.List           as List
import           GHCJS.Marshal.Pure  (pFromJSVal)
import           Common.Prelude
import           System.IO.Unsafe    (unsafePerformIO)



foreign import javascript safe "arg_mount" mountPoint' :: IO JSVal

{-# NOINLINE mountPoint #-}
mountPoint :: String
mountPoint = unsafePerformIO $ fromMaybe "luna-studio-mount" . pFromJSVal <$> mountPoint'

prefix :: JSString -> JSString
prefix name = convert mountPoint <> "-" <> name

isPrefixed :: JSString -> Bool
isPrefixed = List.isPrefixOf (convert $ prefix def) . JSString.unpack
