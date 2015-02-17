{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as ByteString
import           Data.ByteString.Lazy (ByteString)
import           Data.Binary          (Binary)
import           Data.IP              (IP)
import           Data.Word            (Word16)
import           Control.Lens

----------------------------------------------------------------------
-- Address
----------------------------------------------------------------------

data Address a m = Address { _netAddress :: a 
                           , _mask       :: m
                           } deriving (Show)

class HasAddress t a | t -> a where
    address :: Simple Lens t a

----------------------------------------------------------------------
-- Other
----------------------------------------------------------------------

data Message a d= Message a d deriving (Show)
makeLenses ''Message



data IPAddress = IPAddress { _ip   :: IP 
                           , _port :: Port
                           } deriving (Show)


--data Gate = Gate

data Gate = Gate { sndrs :: [Socket ()]
                 , rcvrs :: [Socket ()]
                 } deriving (Show)

data RPC = RPC [Gate]

data Socket m = Socket IPAddress (Maybe m) deriving (Show)


newtype Port = Port Word16 deriving (Show, Num)


--main = do
--    let g   = Gate [Socket (IPAddress "127.0.0.1" 5555) Nothing] []
--        rpc = RPC [g]

--    send "hello" 

--    return ()

connectSocket 



newtype Sections a = Sections [Val a]

data Val a = Single a
           | Range (Maybe a) (Maybe a)


--send :: Binary c => Message a c -> ()
--send (Message a c) = ()

--class Gate g where
--    send :: Message a d -> g -> IO ()




--send :: Message a c -> gate -> IO ()
----send msg g = do

--    return ()