module JS.Converters where

import Data.IORef    ( IORef, newIORef )
import Data.Maybe    ( catMaybes, fromJust )
import GHCJS.Types   ( JSRef, JSArray, JSString )
import GHCJS.Foreign ( fromArray, toArray )
import GHCJS.Marshal ( FromJSRef, ToJSRef, fromJSRef, toJSRef )

fromJSArray :: FromJSRef a => JSArray a -> IO [a]
fromJSArray jsArray = do
  array <- fromArray jsArray
  list <- mapM fromJSRef array
  return $ catMaybes list

toJSArray :: ToJSRef a => [a] -> IO (JSArray a)
toJSArray array = do
  arrayJSRefs <- mapM toJSRef array
  toArray arrayJSRefs

getFromJSRef :: FromJSRef a => IO (JSRef a) -> IO a
getFromJSRef jsRef = fmap fromJust $ jsRef >>= fromJSRef

getListFromJSArray :: FromJSRef a => IO (JSArray a) -> IO [a]
getListFromJSArray jsArray = jsArray >>= fromJSArray

getTuple2FromJSArray :: FromJSRef a => IO (JSArray a) -> IO (a, a)
getTuple2FromJSArray jsArray = do
  a : b : _ <- getListFromJSArray jsArray
  return (a, b)

getTuple3FromJSArray :: FromJSRef a => IO (JSArray a) -> IO (a, a, a)
getTuple3FromJSArray jsArray = do
  a : b : c : _ <- getListFromJSArray jsArray
  return (a, b, c)

getTuple4FromJSArray :: FromJSRef a => IO (JSArray a) -> IO (a, a, a, a)
getTuple4FromJSArray jsArray = do
  a : b : c : d : _ <- getListFromJSArray jsArray
  return (a, b, c, d)

getTuple5FromJSArray :: FromJSRef a => IO (JSArray a) -> IO (a, a, a, a, a)
getTuple5FromJSArray jsArray = do
  a : b : c : d : e : _ <- getListFromJSArray jsArray
  return (a, b, c, d, e)
