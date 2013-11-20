---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

{-# OPTIONS_GHC -fno-warn-orphans  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Flowbox.Luna.Tools.Serialize.Proto.Conversion.Expr where

import           Control.Applicative                                 
import qualified Data.Map                                          as Map
import qualified Text.ProtocolBuffers.Extensions                   as Extensions
import           Flowbox.Prelude                                     
import           Flowbox.Control.Error                               
import qualified Flowbox.Luna.Data.AST.Expr                        as Expr
import           Flowbox.Luna.Data.AST.Expr                          (Expr)
import           Flowbox.Luna.Tools.Serialize.Proto.Conversion.Pat   ()
import           Flowbox.Tools.Serialize.Proto.Conversion.Basic      
import qualified Generated.Proto.Expr.Accessor                     as GenAccessor
import qualified Generated.Proto.Expr.App                          as GenApp
import qualified Generated.Proto.Expr.AppCons_                     as GenAppCons_
import qualified Generated.Proto.Expr.Arg                          as GenArg
import qualified Generated.Proto.Expr.Assignment                   as GenAssignment
import qualified Generated.Proto.Expr.Class                        as GenClass
import qualified Generated.Proto.Expr.Con                          as GenCon
import qualified Generated.Proto.Expr.Expr                         as Gen
import qualified Generated.Proto.Expr.Expr.Cls                     as GenCls
import qualified Generated.Proto.Expr.Field                        as GenField
import qualified Generated.Proto.Expr.Function                     as GenFunction
import qualified Generated.Proto.Expr.Import                       as GenImport
import qualified Generated.Proto.Expr.Infix                        as GenInfix
import qualified Generated.Proto.Expr.Lambda                       as GenLambda
import qualified Generated.Proto.Expr.List                         as GenList
import qualified Generated.Proto.Expr.Lit                          as GenLit
import qualified Generated.Proto.Expr.NOP                          as GenNOP
import qualified Generated.Proto.Expr.Native                       as GenNative
import qualified Generated.Proto.Expr.NativeCode                   as GenNativeCode
import qualified Generated.Proto.Expr.NativeVar                    as GenNativeVar
import qualified Generated.Proto.Expr.RangeFrom                    as GenRangeFrom
import qualified Generated.Proto.Expr.RangeFromTo                  as GenRangeFromTo
import qualified Generated.Proto.Expr.Tuple                        as GenTuple
import qualified Generated.Proto.Expr.Typed                        as GenTyped
import qualified Generated.Proto.Expr.Var                          as GenVar
import qualified Generated.Proto.Expr.Wildcard                     as GenWildcard



genExpr :: GenCls.Cls -> Extensions.Key Maybe Gen.Expr v -> v -> Gen.Expr
genExpr cls key ext = Extensions.putExt key (Just ext)
                    $ Gen.Expr cls $ Extensions.ExtField Map.empty


instance Convert Expr Gen.Expr where
    encode t = case t of 
        Expr.NOP        i          -> genExpr GenCls.NOP GenNOP.ext $ GenNOP.NOP 
                                      (encodePJ i)
        Expr.Accessor   i name dst -> genExpr GenCls.Accessor GenAccessor.ext $ GenAccessor.Accessor 
                                      (encodePJ i) (encodePJ name) (encodeJ dst)
        Expr.App        i src args -> genExpr GenCls.App GenApp.ext $ GenApp.App 
                                      (encodePJ i) (encodeJ src) (encodeList args)
        Expr.AppCons_   i args     -> genExpr GenCls.AppCons_ GenAppCons_.ext $ GenAppCons_.AppCons_ 
                                      (encodePJ i) (encodeList args)
        Expr.Assignment i pat dst  -> genExpr GenCls.Assignment GenAssignment.ext $ GenAssignment.Assignment 
                                      (encodePJ i) (encodeJ pat) (encodeJ dst)
        Expr.Class      i cls classes fields methods 
                                   -> genExpr GenCls.Class GenClass.ext $ GenClass.Class 
                                      (encodePJ i) (encodeJ cls) (encodeList classes) (encodeList fields) (encodeList methods)
        Expr.Con        i name     -> genExpr GenCls.Con GenCon.ext $ GenCon.Con 
                                      (encodePJ i) (encodePJ name)
        Expr.Function   i path name inputs output body   
                                   -> genExpr GenCls.Function GenFunction.ext $ GenFunction.Function 
                                      (encodePJ i) (encodeListP path) (encodePJ name) (encodeList inputs) (encodeJ output) (encodeList body)
    decode t@(Gen.Expr cls _) = case cls of 
        GenCls.NOP -> do ext <- Extensions.getExt GenNOP.ext t
                         (GenNOP.NOP mtid) <- ext <?> "Failed to decode Expr.NOP: extension is missing"
                         tid <- mtid <?> "Failed to decode Expr.NOP: 'id' field is missing"
                         pure $ Expr.NOP (decodeP tid)




--           | Accessor    { id :: ID, name      :: String   , dst       :: Expr                                              }
--           | App         { id :: ID, src       :: Expr     , args      :: [Expr]                                            }
--           | AppCons_    { id :: ID, args      :: [Expr]                                                                    }
--           | Assignment  { id :: ID, pat       :: Pat      , dst       :: Expr                                              }
--           | Class       { id :: ID, cls       :: Type     , classes   :: [Expr] , fields    :: [Expr] , methods :: [Expr]  }
--           | Con         { id :: ID, name      :: String                                                                    }

--           | Function    { id :: ID, path      :: [String] , name      :: String , inputs    :: [Expr] , output    :: Type   ,  body    :: [Expr] }
--           | Lambda      { id :: ID, inputs    :: [Expr]   , output    :: Type   , body      :: [Expr]                      }
--           | Import      { id :: ID, path      :: [String] , target    :: Expr   , rename    :: Maybe String                }
--           | Infix       { id :: ID, name      :: String   , src       :: Expr   , dst       :: Expr                        }                                                               
--           | List        { id :: ID, items     :: [Expr]                                                                    }
--           | Lit         { id :: ID, lvalue    :: Lit                                                                       }
--           | Tuple       { id :: ID, items     :: [Expr]                                                                    }
--           | Typed       { id :: ID, cls       :: Type     , expr      :: Expr                                              }
--           | Var         { id :: ID, name      :: String                                                                    }
--           | Wildcard    { id :: ID                                                                                         }
                         
--           | RangeFromTo { id :: ID, start     :: Expr     , end       :: Expr                                              }
--           | RangeFrom   { id :: ID, start     :: Expr                                                                      }
--           | Field       { id :: ID, name      :: String   , cls       :: Type   , value     :: Maybe Expr                  }
--           | Arg         { id :: ID, pat       :: Pat      , value     :: Maybe Expr                                        }
--           | Native      { id :: ID, segments  :: [Expr]                                                                    }
--           | NativeCode  { id :: ID, code      :: String }
--           | NativeVar   { id :: ID, name      :: String }
