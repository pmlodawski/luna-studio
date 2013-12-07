---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}

module Flowbox.System.Console.StyledText.StyledText where

import           Flowbox.Prelude                         hiding (print)
import qualified Flowbox.Prelude                         as Prelude
import           Flowbox.System.Console.StyledText.Style (Style (Style))
import qualified Flowbox.System.Console.StyledText.Style as Style

import           Data.String         (IsString, fromString)
import qualified Data.Text           as T
import qualified System.Console.ANSI as ANSI
import           System.IO           (stdout)

data Element = Text T.Text
             | StylePush Style
             | StylePop
             deriving (Show)

type Text = [Element]


printStack :: [Style] -> Text -> IO ()
printStack stack []     = putStrLn ""
printStack stack (x:xs) = case x of
    Text txt    -> putStr (T.unpack txt) *> printStack stack xs
    StylePush s -> ANSI.hSetSGR stdout (Style.toSGR s) *> printStack (s:stack) xs
    StylePop    -> case stack of
                   s:ss:sss -> ANSI.hSetSGR stdout (Style.toSGR ss)          *> printStack (ss:sss) xs
                   s:[]     -> ANSI.hSetSGR stdout (Style.toSGR Style.Reset) *> printStack [] xs
                   []       -> printStack [] xs


beginBlack :: Text
beginBlack = [StylePush $ Style Style.Foreground Style.Vivid Style.Black]

beginRed :: Text
beginRed = [StylePush $ Style Style.Foreground Style.Vivid Style.Red]

beginGreen :: Text
beginGreen = [StylePush $ Style Style.Foreground Style.Vivid Style.Green]

beginYellow :: Text
beginYellow = [StylePush $ Style Style.Foreground Style.Vivid Style.Yellow]

beginBlue :: Text
beginBlue = [StylePush $ Style Style.Foreground Style.Vivid Style.Blue]

beginMagenta :: Text
beginMagenta = [StylePush $ Style Style.Foreground Style.Vivid Style.Magenta]

beginCyan :: Text
beginCyan = [StylePush $ Style Style.Foreground Style.Vivid Style.Cyan]

beginWhite :: Text
beginWhite = [StylePush $ Style Style.Foreground Style.Vivid Style.White]

resetColor :: Text
resetColor = [StylePush Style.Reset]

popColor :: Text
popColor = [StylePop]

black :: Text -> Text
black s = beginBlack ++ s ++ popColor

red :: Text -> Text
red s = beginRed ++ s ++ popColor

green :: Text -> Text
green s = beginGreen ++ s ++ popColor

yellow :: Text -> Text
yellow s = beginYellow ++ s ++ popColor

blue :: Text -> Text
blue s = beginBlue ++ s ++ popColor

magenta :: Text -> Text
magenta s = beginMagenta ++ s ++ popColor

cyan :: Text -> Text
cyan s = beginCyan ++ s ++ popColor

white :: Text -> Text
white s = beginWhite ++ s ++ popColor

print :: Text -> IO ()
print txt = printStack [] txt


------------------------------------------------------------------------
-- INSTANCES
------------------------------------------------------------------------

instance IsString Element where
    fromString s = Text (fromString s)

instance IsString Text where
    fromString s = [fromString s]

