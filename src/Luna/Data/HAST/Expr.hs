---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Luna.Data.HAST.Expr where

import           Flowbox.Prelude
import           Luna.Data.HAST.Comment   (Comment)
import           Luna.Data.HAST.Deriving  (Deriving)
import           Luna.Data.HAST.Extension (Extension)
import qualified Luna.Data.HAST.Lit       as Lit
import           Data.Text.Lazy (Text)


type Lit = Lit.Lit

data Expr = Assignment { src       :: Expr     , dst       :: Expr                                }
          | Arrow      { src       :: Expr     , dst       :: Expr                                }
          | Tuple      { items :: [Expr]                                                          }
          | TupleP     { items :: [Expr]                                                          }
          | ListE      { items :: [Expr]                                                          }
          | ListT      { item  :: Expr                                                            }
          | StringLit  { litval :: Text                                                         }
          | Var        { name :: Text                                                           }
          | VarE       { name :: Text                                                           }
          | VarT       { name :: Text                                                           }
          | LitT       { lval :: Lit                                                              }
          | Typed      { cls       :: Expr     , expr      :: Expr                                }
          | TypedP     { cls       :: Expr     , expr      :: Expr                                }
          | TypedE     { cls       :: Expr     , expr      :: Expr                                }
          | TySynD     { name      :: Text   , paramsE   :: [Expr]      , dstType   :: Expr     } -- FIXME: paramsE -> params
          | Function   { name      :: Text   , pats      :: [Expr]      , expr      :: Expr     }
          | Lambda     { paths     :: [Expr]   , expr      :: Expr                                }
          | LetBlock   { exprs     :: [Expr]   , result    :: Expr                                }
          | LetExpr    { expr      :: Expr                                                        }
          | DoBlock    { exprs :: [Expr]                                                          }
          | DataD      { name      :: Text   , params    :: [Text]    , cons      :: [Expr] , derivings :: [Deriving]   }
          | NewTypeD   { name      :: Text   , paramsE   :: [Expr]      , con       :: Expr     } -- FIXME: paramsE -> params
          | InstanceD  { tp        :: Expr     , decs      :: [Expr]                              }
          | Con        { name      :: Text   , fields    :: [Expr]                              }
          | ConE       { qname :: [Text]                                                        }
          | ConT       { name :: Text                                                           }
          | ConP       { name :: Text                                                           }
          | CondE      { cond :: Expr , success :: [Expr], failure :: [Expr]                      }
          | RecUpdE    { expr :: Expr , name :: Text, val :: Expr}
          -- | Module     { path      :: [Text] , ext       :: [Extension] , imports   :: [Expr]   , newtypes  :: [Expr]       , datatypes :: [Expr]  , methods :: [Expr] , thexpressions :: [Expr] }
          | Module     { path      :: [Text] , ext       :: [Extension] , imports   :: [Expr]   , body  :: [Expr]}
          | Import     { qualified :: Bool     , segments  :: [Text]    , rename    :: Maybe Text                           }
          | ImportNative { code :: Text                                                      }
          | AppE       { src       :: Expr     , dst       :: Expr                                }
          | AppT       { src       :: Expr     , dst       :: Expr                                }
          | AppP       { src       :: Expr     , dst       :: Expr                                }
          | Infix      { name      :: Text   , src       :: Expr        , dst          :: Expr  }
          | Lit        { lval :: Lit                                                              }
          | Native     { code :: Text                                                           }
          | THE        { expr :: Expr                                                             }
          | CaseE      { expr :: Expr, matches :: [Expr]                                          }
          | Match      { pat :: Expr, matchBody :: Expr }
          | Comment    { comment :: Comment }
          | ViewP      { name :: Text, dst :: Expr} 
          | WildP
          | RecWildP
          | NOP
          | Undefined
          | Bang Expr
          deriving (Show)


