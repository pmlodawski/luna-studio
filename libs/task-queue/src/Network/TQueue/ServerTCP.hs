{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Server where

import Network.Transport      hiding (Connection, address)
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import qualified Network.Transport as Transport
import Control.Concurrent
import Data.Map
import Control.Exception
import System.Environment
import Data.Binary (Binary, decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Default
import Control.Lens
import Control.Monad.IO.Class (MonadIO, liftIO)
import Worker.Class (Worker(Worker))
import GHC.Generics (Generic)
import Message

import Control.Distributed.Process hiding (Message, handleMessage)
import Control.Distributed.Process.Node

type TConnection = Transport.Connection

instance Show (MVar a) where
    show _ = "MVar"


data Server = Server { _connections :: Map ConnectionId Connection
                     , _addresses   :: Map EndPointAddress [ConnectionId]
                     , _orphans     :: [EndPointAddress] -- zrobic sequencje z czasami gdy padlo
                     , _pool        :: Pool
                     } deriving (Show)

data Pool = Pool { _free :: Map ConnectionId Worker 
                 , _busy :: Map ConnectionId Worker 
                 } deriving (Show)

data Connection = Connection { _transport :: MVar TConnection
                             , _address   :: EndPointAddress
                             } deriving (Show)


makeLenses ''Server
makeLenses ''Pool
makeLenses ''Connection

instance Default Server where
    def = Server def def def def

instance Default Pool where
    def = Pool def def



insConn :: ConnectionId -> Connection -> Server -> Server
insConn cid conn srv = srv & connections.at cid ?~ conn
                           & addresses.ix addr %~ (cid:)
    where addr = conn^.address

delCon :: ConnectionId -> Server -> Server
delCon cid srv = case addr of
    Nothing -> srv'
    Just a  -> srv' & addresses.at a .~ Nothing
                    -- & orphans ...
    where addr = srv^?(connections.ix cid.address)
          srv' = srv & connections.at cid .~ Nothing
                     & pool.free.at cid   .~ Nothing
                     & pool.busy.at cid   .~ Nothing




handleMessage :: MonadIO m => ConnectionId -> Message -> Server -> m Server
handleMessage cid msg srv = case msg of
    RegisterWorker w -> do
        liftIO . putStrLn $ "New worker " ++ show w
        return $ srv & pool.free.at cid ?~ w

-- | Server that echoes messages straight back to the origin endpoint.
echoServer :: EndPoint -> MVar () -> IO ()
echoServer endpoint serverDone = go def
  where
    go :: Server -> IO ()
    go srv = do
      event <- receive endpoint
      return ()
      case event of
        ConnectionOpened cid rel addr -> do
          putStrLn$ "  New connection: ID "++show cid++", reliability: "++show rel++", address: "++ show addr
          connMVar <- newEmptyMVar
          forkIO $ do
            Right conn <- connect endpoint addr rel defaultConnectHints
            putMVar connMVar conn
          go (insConn cid (Connection connMVar addr) srv)
        Received cid rmsg -> do
          let [msg] = fmap (decode . fromStrict) rmsg :: [Message]
          srv' <- handleMessage cid msg srv
          go srv'
        ConnectionClosed cid -> do
          putStrLn$ "    Closed connection: ID "++show cid
          let Just trans = srv^?(connections.ix cid.transport)
          forkIO $ do
            conn <- readMVar trans
            close conn
          go (delCon cid srv)
        EndPointClosed -> do
          putStrLn "Echo server exiting"
          putMVar serverDone ()
        ErrorEvent e -> case e of
          TransportError te msg -> case te of
            EventConnectionLost addr -> do print $ "LOST! " ++ show addr ++ " :: " ++ msg
                                           go srv
        _ -> do
          print $ "Other error: " ++ show event


onCtrlC :: IO a -> IO () -> IO a
p `onCtrlC` q = catchJust isUserInterrupt p (const $ q >> p `onCtrlC` q)
  where
    isUserInterrupt :: AsyncException -> Maybe ()
    isUserInterrupt UserInterrupt = Just ()
    isUserInterrupt _             = Nothing



main :: IO ()
main = do
  Right t <- createTransport "127.0.0.17" "10501" defaultTCPParameters
  node <- newLocalNode t initRemoteTable

  -- in main
  _ <- forkProcess node $ do
    self <- getSelfPid
    register "serverPID" self

  [host, port]    <- getArgs
  serverDone      <- newEmptyMVar
  Right transport <- createTransport host port defaultTCPParameters
  Right endpoint  <- newEndPoint transport
  forkIO $ echoServer endpoint serverDone
  putStrLn $ "Echo server started at " ++ show (Transport.address endpoint)
  readMVar serverDone `onCtrlC` closeTransport transport
