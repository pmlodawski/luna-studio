module Reactive.Plugins.Core.Action.Backend.Common
    ( whenOk
    , handleResponse
    , doNothing
    ) where


import qualified Data.Aeson                as JSON (ToJSON, encode)
import           Data.Text.Encoding        (decodeUtf8)
import qualified Data.Text.Lazy            as Text
import qualified Data.UUID.Types           as UUID (toString)
import           Utils.PreludePlus

import qualified Batch.Workspace           as Workspace
import           Event.Batch               (Event (..))
import qualified Event.Batch               as Batch
import qualified Event.Event               as Event
import           Reactive.Commands.Command (Command, performIO)
import           Reactive.Commands.UUID    (isOwnRequest, unregisterRequest)
import           Reactive.State.Global     (State)
import qualified Reactive.State.Global     as Global

import qualified Empire.API.Response       as Response
import qualified Empire.API.Topic          as Topic
import qualified JS.Debug                  as Debug

whenOk :: Response.Response req res -> (res -> Command State ()) -> Command State ()
whenOk (Response.Response _ req (Response.Ok res))  handler = handler res
whenOk (Response.Response _ req (Response.Error _)) _       = return ()

handleResponse :: (Topic.MessageTopic (Response.Response req res), JSON.ToJSON req) => Response.Response req res -> (req -> res -> Command State ()) -> Command State ()
handleResponse resp@(Response.Response uuid req status) success = do
    whenM (isOwnRequest uuid) $ do
        unregisterRequest uuid
        case status of
            Response.Ok result -> success req result
            Response.Error str -> performIO $ Debug.error (Text.pack $ (Topic.topic resp) <> " [" <> (UUID.toString uuid) <> "] " <> str) req

doNothing :: a -> b -> Command State ()
doNothing _ _ = return ()
