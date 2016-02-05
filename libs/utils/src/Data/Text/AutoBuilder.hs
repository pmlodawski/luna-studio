{-# LANGUAGE NoMonomorphismRestriction #-}

module Data.Text.AutoBuilder where

import           Flowbox.Prelude

import           Data.Convert
import           Data.IORef
import           Data.Text.Lazy         (Text)
import qualified Data.Text.Lazy         as T
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import           System.IO.Unsafe

newtype AutoBuilder = AutoBuilder (IORef Data)

data Data = Text Text
          | Builder Builder



singleton :: Char -> AutoBuilder
singleton = convert . Builder . TB.singleton

instance Monoid AutoBuilder where
    mempty           = convert $ Text mempty
    ab `mappend` ab' = convert $ case (convert ab, convert ab') of
        (Text    t, Text    t') -> Text    $ t <> t'
        (Text    t, Builder t') -> Builder $ TB.fromLazyText t <> t'
        (Builder t, Text    t') -> Builder $ t <> TB.fromLazyText t'
        (Builder t, Builder t') -> Builder $ t <> t'

instance Show AutoBuilder where
    show = show . toText

instance IsString AutoBuilder where
    fromString = convert . Text . fromString

instance Convertible Data AutoBuilder where
    {-# NOINLINE convert #-}
    convert = AutoBuilder . unsafePerformIO . newIORef

instance Convertible AutoBuilder Data where
    {-# NOINLINE convert #-}
    convert (AutoBuilder ref) = unsafePerformIO $ readIORef ref

instance FromText AutoBuilder where
    fromText = convert . Text

instance ToText AutoBuilder where
    {-# NOINLINE toText #-}
    toText (AutoBuilder ref) = unsafePerformIO $ do
        d <- readIORef ref
        case d of
            Text    t -> return $ t
            Builder b -> do
                let t = TB.toLazyText b
                atomicWriteIORef ref $ Text t
                return t


--main = do
--    let a = fromString (replicate 1000 'a') :: AutoBuilder
--        b = fromString (replicate 1000 'b') :: AutoBuilder
--        c = a <> b
--        --d = c <> a

--    print $ c
--    --print $ d
