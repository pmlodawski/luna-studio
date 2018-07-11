{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Empire.Data.Parser where

-- import           Empire.Prelude       hiding (Type, mempty)

-- import           Control.Monad.Catch  (MonadCatch(..))
-- import           Control.Monad.State  (StateT, runStateT, get, put)
-- import qualified Control.Monad.State.Dependent as DepState
-- import qualified Data.Map             as Map
-- import           Data.Foldable        (toList)
-- import           Empire.Data.Graph    (AST(..), Graph, withVis)
-- import qualified Empire.Data.Graph    as Graph (ast, breadcrumbHierarchy)
-- import qualified Empire.Data.BreadcrumbHierarchy as BH
-- import           Empire.Data.Layers   (Marker, Meta, TypeLayer, SpanOffset, SpanLength)
-- import           Empire.Empire        (Command)

-- import           Luna.IR              hiding (Marker, get, put, match)
-- import qualified Luna.Pass        as Pass

-- import           Luna.Pass.Data.ExprRoots                     (ExprRoots(..))
-- import           Luna.Syntax.Text.Parser.Data.Invalid               (Invalids)

-- import qualified Parser.Parser               as Parser
-- import qualified Parser.Parsing              as Parsing
-- import           Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
-- import           Luna.Syntax.Text.Parser.Marker               (MarkedExprMap)
-- import           Luna.Syntax.Text.Source                      (Source)



-- data ParserPass
-- type instance Pass.Spec ParserPass t = ParserPassSpec t
-- type family ParserPassSpec t where
--     ParserPassSpec (Pass.In  Pass.Attrs)  = '[Invalids, Source {-, Parser.ParsedExpr, MarkedExprMap, Parser.ReparsingStatus -}]
--     ParserPassSpec (Pass.Out Pass.Attrs)  = '[Invalids, Source {-, Parser.ParsedExpr, MarkedExprMap, Parser.ReparsingStatus -}]
--     ParserPassSpec (Pass.In  AnyExpr)     = '[Model, Type, Users, SpanLength, CodeSpan]
--     ParserPassSpec (Pass.Out AnyExpr)     = '[Model, Type, Users, SpanLength]
--     ParserPassSpec (Pass.In  AnyExprLink) = '[SpanOffset, Source, Target]
--     ParserPassSpec (Pass.Out AnyExprLink) = '[SpanOffset, Source, Target]
--     ParserPassSpec t                      = Pass.BasicPassSpec t
