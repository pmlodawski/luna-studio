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

data Expr = DataD        { _name      :: Text     , _params    :: [Text]      , _cons    :: [Expr]     , _derivings :: [Deriving] }
          | Module       { _path      :: [Text]   , _ext       :: [Extension] , _imports :: [Expr]     , _body  :: [Expr]         }
          | TySynD       { _name      :: Text     , _paramsE   :: [Expr]      , _dstType :: Expr                                  } -- FIXME: paramsE -> params
          | Function     { _name      :: Text     , _pats      :: [Expr]      , _expr    :: Expr                                  }
          | NewTypeD     { _name      :: Text     , _paramsE   :: [Expr]      , _con     :: Expr                                  } -- FIXME: paramsE -> params
          | CondE        { _cond      :: Expr     , _success   :: [Expr]      , _failure :: [Expr]                                }
          | RecUpdE      { _expr      :: Expr     , _name      :: Text        , _val     :: Expr                                  }
          | Import       { _qualified :: Bool     , _segments  :: [Text]      , _rename  :: Maybe Text                            }
          | Infix        { _name      :: Text     , _src       :: Expr        , _dst     :: Expr                                  }
          | InfixR       { _name      :: Text     , _src       :: Expr        , _dst     :: Expr                                  }
          | Assignment   { _src       :: Expr     , _dst       :: Expr                                                            }
          | Arrow        { _src       :: Expr     , _dst       :: Expr                                                            }
          | Typed        { _cls       :: Expr     , _expr      :: Expr                                                            }
          | TypedP       { _cls       :: Expr     , _expr      :: Expr                                                            }
          | TypedE       { _cls       :: Expr     , _expr      :: Expr                                                            }
          | Lambda       { _paths     :: [Expr]   , _expr      :: Expr                                                            }
          | LetBlock     { _exprs     :: [Expr]   , _result    :: Expr                                                            }
          | InstanceD    { _tp        :: Expr     , _decs      :: [Expr]                                                          }
          | Con          { _name      :: Text     , _fields    :: [Expr]                                                          }
          | AppE         { _src       :: Expr     , _dst       :: Expr                                                            }
          | AppT         { _src       :: Expr     , _dst       :: Expr                                                            }
          | AppP         { _src       :: Expr     , _dst       :: Expr                                                            }
          | CaseE        { _expr      :: Expr     , _matches   :: [Expr]                                                          }
          | Match        { _pat       :: Expr     , _matchBody :: Expr                                                            }
          | ViewP        { _name      :: Text     , _dst       :: Expr                                                            } 
          | Tuple        { _items     :: [Expr]                                                                                   }
          | TupleP       { _items     :: [Expr]                                                                                   }
          | ListE        { _items     :: [Expr]                                                                                   }
          | ListT        { _item      :: Expr                                                                                     }
          | StringLit    { _litval    :: Text                                                                                     }
          | Var          { _name      :: Text                                                                                     }
          | VarE         { _name      :: Text                                                                                     }
          | VarT         { _name      :: Text                                                                                     }
          | LitT         { _lval      :: Lit                                                                                      }
          | LetExpr      { _expr      :: Expr                                                                                     }
          | DoBlock      { _exprs     :: [Expr]                                                                                   }
          | ConT         { _name      :: Text                                                                                     }
          | ConP         { _name      :: Text                                                                                     }
          | ConE         { _qname     :: [Text]                                                                                   }
          | ImportNative { _code      :: Text                                                                                     }
          | Native       { _code      :: Text                                                                                     }
          | THE          { _expr      :: Expr                                                                                     }
          | Lit          { _lval      :: Lit                                                                                      }
          | Comment      { _comment   :: Comment                                                                                  }
          | WildP
          | RecWildP
          | NOP
          | Undefined
          | Bang Expr
          deriving (Show)


proxy name = Typed (AppT (VarT "Proxy") (Lit $ Lit.String name)) $ VarE "Proxy"
app        = foldl AppE 
rtuple = foldr cons (Tuple [])
    where cons = InfixR "<:>"


val = flip Function mempty



