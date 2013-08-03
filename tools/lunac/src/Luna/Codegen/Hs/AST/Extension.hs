---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2013
---------------------------------------------------------------------------

module Luna.Codegen.Hs.AST.Extension (
    Extension(..),
    genCode
)where

import           Data.String.Utils                 (join)

data Extension = AlternativeLayoutRule                
               | AlternativeLayoutRuleTransitional    
               | Arrows                               
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
               | NoAlternativeLayoutRule              
               | NoAlternativeLayoutRuleTransitional  
               | NoArrows                             
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
               | NoEmptyDataDecls                     
               | NoExistentialQuantification          
               | NoExplicitForAll                     
               | NoExplicitNamespaces                 
               | NoExtendedDefaultRules               
               | NoFlexibleContexts                   
               | NoFlexibleInstances                  
               | NoImpredicativeTypes
               | NoIncoherentInstances
               | NoInstanceSigs
               | NoInterruptibleFFI
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
               | NoNondecreasingIndentation
               | NoOverlappingInstances
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
               | NoScopedTypeVariables
               | NoStandaloneDeriving
               | NoTemplateHaskell
               | NoTraditionalRecordSyntax
               | NoTransformListComp
               | NoTupleSections
               | NoTypeFamilies
               | NoTypeOperators
               | NoTypeSynonymInstances
               | NoUnboxedTuples
               | NoUndecidableInstances
               | NoUnicodeSyntax
               | NoUnliftedFFITypes
               | NoViewPatterns
               | NondecreasingIndentation
               | OverlappingInstances
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
               | Safe
               | ScopedTypeVariables
               | StandaloneDeriving
               | TemplateHaskell
               | TraditionalRecordSyntax
               | TransformListComp
               | Trustworthy
               | TupleSections
               | TypeFamilies
			   deriving (Show)


genCode :: [Extension] -> String
genCode exts = case exts of
	[] -> ""
	_  -> "{-# LANGUAGE " ++ join ", " (map show exts) ++ " #-}\n\n"









