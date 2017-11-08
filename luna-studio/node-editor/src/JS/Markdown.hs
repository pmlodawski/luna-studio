module JS.Markdown where

import           Common.Prelude

foreign import javascript safe "new showdown.Converter().makeHtml($1)" mdToHtml' :: JSString -> IO JSString

mdToHtml :: Text -> IO Text
mdToHtml = convert . mdToHtml' . convert
