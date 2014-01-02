---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Flowbox.Luna.Data.HAST.Extension (
    Extension(..),
    genCode
)where

import Data.String.Utils (join)
import Flowbox.Prelude

data Extension = AllowAmbiguousTypes                  
               | AlternativeLayoutRule                
               | AlternativeLayoutRuleTransitional    
               | Arrows                               
               | AutoDeriveTypeable                   
               | BangPatterns                         
               | CApiFFI                              
               | CPP                                  
               | ConstrainedClassMethods              
               | ConstraintKinds                      
               | DataKinds                            
               | DatatypeContexts                     
               | DefaultSignatures                    
               | DeriveDataTypeable                   
               | DeriveFoldable                       
               | DeriveFunctor                        
               | DeriveGeneric                        
               | DeriveTraversable                    
               | DisambiguateRecordFields             
               | DoAndIfThenElse                      
               | DoRec                                
               | EmptyCase                            
               | EmptyDataDecls                       
               | ExistentialQuantification            
               | ExplicitForAll                       
               | ExplicitNamespaces                   
               | ExtendedDefaultRules                 
               | FlexibleContexts                     
               | FlexibleInstances                    
               | ForeignFunctionInterface             
               | FunctionalDependencies               
               | GADTSyntax                           
               | GADTs                                
               | GHCForeignImportPrim                 
               | GeneralizedNewtypeDeriving           
               | Generics                             
               | Haskell2010                          
               | Haskell98                            
               | ImplicitParams                       
               | ImplicitPrelude                      
               | ImpredicativeTypes                   
               | IncoherentInstances                  
               | InstanceSigs                         
               | InterruptibleFFI                     
               | JavaScriptFFI                        
               | KindSignatures                       
               | LambdaCase                           
               | LiberalTypeSynonyms                  
               | MagicHash                            
               | MonadComprehensions                  
               | MonoLocalBinds                       
               | MonoPatBinds                         
               | MonomorphismRestriction              
               | MultiParamTypeClasses                
               | MultiWayIf                           
               | NPlusKPatterns                       
               | NamedFieldPuns                       
               | NegativeLiterals                     
               | NoAllowAmbiguousTypes                
               | NoAlternativeLayoutRule              
               | NoAlternativeLayoutRuleTransitional  
               | NoArrows                             
               | NoAutoDeriveTypeable                 
               | NoBangPatterns                       
               | NoCApiFFI                            
               | NoCPP                                
               | NoConstrainedClassMethods            
               | NoConstraintKinds                    
               | NoDataKinds                          
               | NoDatatypeContexts                   
               | NoDefaultSignatures                  
               | NoDeriveDataTypeable                 
               | NoDeriveFoldable                     
               | NoDeriveFunctor                      
               | NoDeriveGeneric                      
               | NoDeriveTraversable                  
               | NoDisambiguateRecordFields           
               | NoDoAndIfThenElse                    
               | NoDoRec                              
               | NoEmptyCase                          
               | NoEmptyDataDecls                     
               | NoExistentialQuantification          
               | NoExplicitForAll                     
               | NoExplicitNamespaces                 
               | NoExtendedDefaultRules               
               | NoFlexibleContexts                   
               | NoFlexibleInstances                  
               | NoForeignFunctionInterface           
               | NoFunctionalDependencies             
               | NoGADTSyntax                         
               | NoGADTs                              
               | NoGHCForeignImportPrim               
               | NoGeneralizedNewtypeDeriving         
               | NoGenerics                           
               | NoImplicitParams                     
               | NoImplicitPrelude                    
               | NoImpredicativeTypes                 
               | NoIncoherentInstances
               | NoInstanceSigs
               | NoInterruptibleFFI
               | NoJavaScriptFFI
               | NoKindSignatures
               | NoLambdaCase
               | NoLiberalTypeSynonyms
               | NoMagicHash
               | NoMonadComprehensions
               | NoMonoLocalBinds
               | NoMonoPatBinds
               | NoMonomorphismRestriction
               | NoMultiParamTypeClasses
               | NoMultiWayIf
               | NoNPlusKPatterns
               | NoNamedFieldPuns
               | NoNegativeLiterals
               | NoNondecreasingIndentation
               | NoNullaryTypeClasses
               | NoNumDecimals
               | NoOverlappingInstances
               | NoOverloadedLists
               | NoOverloadedStrings
               | NoPackageImports
               | NoParallelArrays
               | NoParallelListComp
               | NoPatternGuards
               | NoPatternSignatures
               | NoPolyKinds
               | NoPolymorphicComponents
               | NoPostfixOperators
               | NoQuasiQuotes
               | NoRank2Types
               | NoRankNTypes
               | NoRebindableSyntax
               | NoRecordPuns
               | NoRecordWildCards
               | NoRecursiveDo
               | NoRelaxedLayout
               | NoRelaxedPolyRec
               | NoRoleAnnotations
               | NoScopedTypeVariables
               | NoStandaloneDeriving
               | NoTemplateHaskell
               | NoTraditionalRecordSyntax
               | NoTransformListComp
               | NoTupleSections
               | NoTypeFamilies
               | NoTypeHoles
               | NoTypeOperators
               | NoTypeSynonymInstances
               | NoUnboxedTuples
               | NoUndecidableInstances
               | NoUnicodeSyntax
               | NoUnliftedFFITypes
               | NoViewPatterns
               | NondecreasingIndentation
               | NullaryTypeClasses
               | NumDecimals
               | OverlappingInstances
               | OverloadedLists
               | OverloadedStrings
               | PackageImports
               | ParallelArrays
               | ParallelListComp
               | PatternGuards
               | PatternSignatures
               | PolyKinds
               | PolymorphicComponents
               | PostfixOperators
               | QuasiQuotes
               | Rank2Types
               | RankNTypes
               | RebindableSyntax
               | RecordPuns
               | RecordWildCards
               | RecursiveDo
               | RelaxedLayout
               | RelaxedPolyRec
               | RoleAnnotations
               | Safe
               | ScopedTypeVariables
               | StandaloneDeriving
               | TemplateHaskell
               | TraditionalRecordSyntax
               | TransformListComp
               | Trustworthy
               | TupleSections
               | TypeFamilies
               | TypeHoles
               | TypeOperators
               | TypeSynonymInstances
               | UnboxedTuples
               | UndecidableInstances
               | UnicodeSyntax
               | UnliftedFFITypes
               | Unsafe
               | ViewPatterns
               deriving (Show)


genCode :: [Extension] -> String
genCode exts = case exts of
        [] -> ""
        _  -> "{-# LANGUAGE " ++ join ", " (map show exts) ++ " #-}\n\n"









