{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module TestConfigParser where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Flowbox.Prelude
import           System.Directory

data TestConfig a =
    TestConfig     { host     :: a
                   , port     :: a
                   , rootDir  :: a
                   , user     :: a
                   , password :: a
                    } deriving (Show, Generic, Functor)

instance FromJSON a => FromJSON (TestConfig a)
instance ToJSON a => ToJSON (TestConfig a)

getTestConfig path = do
    jsonDefault <- BS.readFile path
    let eDefConf = (eitherDecode jsonDefault) :: (Either String (TestConfig (Maybe String)))
    case eDefConf of
        Left  _    -> error "no config"
        Right conf -> return $ fmap stringize conf

--getTestConfig = do
--    let def = "./default.config"
--        cus = "./custom.config"

--    defaultExists <- doesFileExist $ def
--    customExists  <- doesFileExist $ cus
--    --if defaultExists
--    jsonDefault <- BS.readFile "./default.config"
--    jsonCustom  <- BS.readFile "./customm.config"
--    --print ((eitherDecode json) :: (Either String TestConfig))
--    let eDefConf = (eitherDecode jsonDefault) :: (Either String (TestConfig (Maybe String)))
--        --eCusConf = (eitherDecode jsonCustom)  :: (Either String (TestConfig (Maybe String)))
--    --print $ eDefConf
--    --print $ eCusConf
--        mergedConf = mergeConfs eDefConf eDefConf

--    return mergedConf
--    --case eDefConf of
--    --    Left  _    -> error "no config"
--    --    Right conf -> return conf

getRemotePath conf = (host conf) ++ (port conf) ++ (rootDir conf)

mergeConfs :: Either String (TestConfig (Maybe String)) -> Either String (TestConfig (Maybe String)) -> TestConfig String
mergeConfs (Left _)    (Left _)     = error "No config file. Yous should have default.config or custom.config in /test directory."
mergeConfs (Right def) (Left _)     = fmap stringize def
mergeConfs (Left _)    (Right cus)  = fmap stringize cus
mergeConfs (Right def) (Right cus)  = TestConfig h p r u pa where
    h:p:r:u:pa:_  = zipWith choseLeft cusList defList
    defList       = map ($ def) fields
    cusList       = map ($ cus) fields
    fields        = [host, port, rootDir, user, password]

stringize :: Maybe String -> String
stringize Nothing  = ""
stringize (Just str) = str

choseLeft :: Maybe String -> Maybe String -> String
choseLeft Nothing  Nothing  = "bad path" --TODO[KR]: What to do about that
choseLeft Nothing  (Just y) = y
choseLeft (Just x) Nothing  = x
choseLeft (Just x) (Just y) = x
