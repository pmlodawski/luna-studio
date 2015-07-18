{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Convert.Instances.TH where

import Data.Convert.Bound
import Data.Convert.Base
import Data.Monoid
import qualified Data.ByteString as BS



intTypes       = fractionals ("Integer" : fracNames "Int")   [Max,8,16,32,64]
wordTypes      = fractionals (fracNames "Word")              [8,16,32,64]
floatTypes     = fractionals ["Rational", "Float", "Double"] [Max,32,64]
charType       = fractional  "Char"                          32
byteStringType = fractional  "ByteString"                    8

integralTypes = intTypes <> wordTypes

numTypes = intTypes <> wordTypes <> floatTypes

--intBase   = mkConversions [| fromIntegral            |] intTypes
--wordBase  = mkConversions [| fromIntegral            |] wordTypes
--floatBase = mkConversions [| truncate                |] floatTypes
--charBase  = mkConversions [| fromIntegral . fromEnum |] charType

--numDescs = [ ( [| fromIntegral            |] , intTypes   )
--           , ( [| fromIntegral            |] , wordTypes  )
--           , ( [| truncate                |] , floatTypes )
--           , ( [| fromIntegral . fromEnum |] , charType   )
--           ]


numConversions =  mkConversions   [| fromIntegral                 |] integralTypes  integralTypes
               <> safeConversions [| fromIntegral                 |] integralTypes  floatTypes
               <> mkConversions   [| truncate                     |] floatTypes     integralTypes
               <> safeConversions [| realToFrac                   |] floatTypes     floatTypes
               <> mkConversions   [| fromIntegral . fromEnum      |] charType       integralTypes
               -- <> mkConversions   [| BS.singleton . unsafeConvert |] integralTypes  byteStringType
               -- <> mkConversions   [| BS.singleton . unsafeConvert |] byteStringType integralTypes
              -- <> wordBase



--numConversions :: [Conversion]
--numConversions = concat $ typeCons <*> fmap snd numDescs where
--    typeCons = fmap (uncurry mkConversions) numDescs