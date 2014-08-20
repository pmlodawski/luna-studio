---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Flowbox Team <contact@flowbox.io>, 2014
-- Proprietary and confidential
-- Unauthorized copying of this file, via any medium is strictly prohibited
---------------------------------------------------------------------------
module Luna.Interpreter.Session.TypeCheck where

import qualified Data.List                    as List
import qualified Language.Haskell.Interpreter as Interpreter

import Flowbox.Prelude
import Flowbox.System.Log.Logger
import Luna.Interpreter.Session.Session (Session)



logger :: LoggerIO
logger = getLoggerIO "Luna.Interpreter.Session.Session"


function :: String -> [String] -> Session String
function funName args = do
    let expr = unwords $ funName : args
    typedArgs   <- mapM typeOf args
    typedResult <- typeOf expr
    let funType =  List.intercalate " -> " $ typedArgs ++ [typedResult]
    return $ funName ++ " :: " ++ funType


variable :: String -> Session String
variable expr = do
    t <- typeOf expr
    return $ "(" ++ expr ++ " :: " ++ t ++ ")"


typeOf :: String -> Session String
typeOf arg = do
    logger trace $ "Checking type of: " ++ show arg
    t <- lift2 $ Interpreter.typeOf arg
    let t2 = case t of
                "Num a => a"        -> "Int"
                "Fractional a => a" -> "Double"
                _                   -> t
    logger trace $ "Result: " ++ show t2
    return t2
