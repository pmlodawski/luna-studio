{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}


module Generator.Cython.CyAST where

import Control.Lens

data CyProgram = CyProgram { _cyClasses   :: [CyClass]
                           , _cyStatWraps :: [CyStatWrap]
                           , _cyFunctions :: [CyFunction]
                           }
               deriving (Show, Eq, Ord)


data CyClass = CyClass { _className         :: String
                       , _classFields       :: [CyField]
                       , _classMethods      :: [CyMethod]
                       , _classConss        :: [CyCons]
                       , _classBases        :: [String]
                       , _classEnums        :: [CyEnum]
                       , _classTemplatePars :: [CyType]
                       }
             deriving (Show, Eq, Ord)


data CyField = CyField { _fieldName :: String
                       , _fieldType :: CyType
                       }
              deriving (Show, Eq, Ord)


data CyType = CyType { _typeName :: String
                     --, _typeTemplates :: [String]
                     }
            deriving (Show, Eq, Ord)


data CyArg = CyArg { _argName :: String
                   , _argType :: CyType
                   }
           deriving (Show, Eq, Ord)


data CyMethod = CyMethod { _methodName    :: String
                         , _methodArgs    :: [CyArg]
                         , _methodRetType :: CyType
                         , _methodStatic  :: Bool
                         } deriving (Show, Eq, Ord)


data CyCons = CyCons { _consName :: String
                     , _consArgs :: [CyArg]
                     }
            deriving (Show, Eq, Ord)


data CyStatWrap = CyStatWrap { _statMethod    :: CyMethod
                             , _statClassName :: String
                             , _statTemplates :: [CyType]
                             }
                deriving (Show, Eq, Ord)


data CyFunction = CyFunction { _functionName    :: String
                             , _functionArgs    :: [CyArg]
                             , _functionRetType :: CyType
                             }
                deriving (Show, Eq, Ord)


data CyEnum = CyEnum { _enumName  :: String
                     , _enumElems :: [String]
                     }
            deriving (Show, Eq, Ord)


makeLenses ''CyProgram
makeLenses ''CyClass
makeLenses ''CyCons
makeLenses ''CyMethod
makeLenses ''CyStatWrap
makeLenses ''CyField
makeLenses ''CyFunction
makeLenses ''CyArg
makeLenses ''CyType
makeLenses ''CyEnum
