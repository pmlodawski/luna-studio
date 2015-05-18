{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NamedFieldPuns #-}

module Generator.FFI where

import Generator.Expr (adjust, Point(..))

import Data.Functor
import Debug.Trace
import Control.Monad

import Data.ByteString.Lazy (ByteString, empty, pack, unpack)
import Language.Haskell.TH
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (VarStrictType)
import qualified Language.Haskell.TH.Syntax as THS

import Data.Binary


class ArgumentTypes a where
    argumentTypes :: a -> [Type]

instance ArgumentTypes Type where
    argumentTypes functionType = case functionType of
        AppT 
            (AppT ArrowT t) 
            (rhs) 
          -> [t] ++ argumentTypes rhs
        _ -> []


callWithArgs :: Name -> [Name] -> Exp
callWithArgs fname [] = VarE fname
callWithArgs fname fargs = AppE (callWithArgs fname $ init fargs) (VarE $ last fargs)

generateFFI :: Name -> Q [Dec]
generateFFI fname = do
    n@(VarI fname ftype _ _) <- reify fname
    let argsTypes = argumentTypes ftype
    -- let argNames = map (\x -> "arg" ++ show x) [0 .. (length argsTypes) - 1] -- [arg0, arg1, ...]
    argNames <- replicateM (length argsTypes) (newName "arg")
    let argPattern = tupP (varP <$> argNames)
    let callExpr = return $ callWithArgs fname argNames :: Q Exp

    let generatedFname = mkName $ nameBase fname ++ "'"
    functionType <- [t| ByteString -> ByteString |]
    let prototype = SigD generatedFname functionType


    binaryArgName <- newName "argBin"
    resultBinName <- newName "resultBin"

    bodyWhereSection <- [d| $(argPattern) = decode $(varE binaryArgName)
                            result = $(callExpr)
                            $(varP resultBinName) = encode result|] 
    let clause = Clause 
                    [VarP binaryArgName] 
                    (NormalB (VarE resultBinName))
                    bodyWhereSection

    let definition = FunD generatedFname [clause]


    --trace ("fname:\t" ++ show argNames) (return [])
    --trace ("fname:\t" ++ show ftype) (return [])
    --trace (show $ argumentTypes ftype) (return [])
    --sample <- [d| adjust' :: ByteString -> ByteString
    --              adjust' argBin = resultBin 
    --                               where
    --                                $(argPattern) = decode argBin
    --                                result = $(callExpr)
    --                                resultBin = encode result|]
    --trace (show sample ++ "\n\n----\n\n" ++ show n) (return [])
    -- return sample

    return [prototype, definition]
    
    

bar = adjust

--main = do
--    return $(generateFFI "")
--    let foo = adjust "" $ [Point 2 4]
--    putStrLn "fff"
---- main = generateFFI ''adjust "ttt"