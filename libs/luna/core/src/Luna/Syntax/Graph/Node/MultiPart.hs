---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}

module Luna.Syntax.Graph.Node.MultiPart where

import           Control.Monad.State
import qualified Data.Maybe          as Maybe

import           Flowbox.Prelude          hiding (mapM)
import           Luna.Data.ASTInfo        (ASTInfo)
import           Luna.Syntax.Expr         (LExpr)
import qualified Luna.Syntax.Expr         as Expr
import           Luna.Syntax.Label        (Label (Label))
import qualified Luna.Syntax.Name         as Name
import qualified Luna.Syntax.Name.Pattern as Pattern
import           Luna.Util.LunaShow       (lunaShow)


type V = ()

data MultiPartExpr a = MultiPartExpr
    { _prefix   :: Bool
    , _base     :: MultiPartSegment (a, InverseExpr)
    , _segments :: [MultiPartSegment Pattern.SegmentName]
    } deriving (Show, Eq, Read)


data MultiPartSegment base = MultiPartSegment
    { _segmentBase :: base
    , _argsCount   :: Int
    } deriving (Show, Eq, Read)


data InverseExpr = Accessor { _accName :: Name.NameBaseP }
                 | Cons     { _conName :: Name.CNameP }
                 | Var      { _variable :: Expr.Variable V }
    deriving (Show, Eq, Read)


makeLenses ''InverseExpr
makeLenses ''MultiPartExpr
makeLenses ''MultiPartSegment

type AppArg a v = Expr.AppArg (LExpr a v)


fromNamePat :: Show a => Pattern.NamePat (LExpr a V) arg -> MultiPartExpr a
fromNamePat (Pattern.NamePat nPrefix nBase nSegmentList) =
    MultiPartExpr (Maybe.isJust nPrefix) (fromBase nBase) (map fromSegment nSegmentList)

fromBase :: Show a => Pattern.Segment (LExpr a V) arg -> MultiPartSegment (a, InverseExpr)
fromBase (Pattern.Segment (Label l expr) nSegmentArgs) =
        MultiPartSegment (l, inverseExpr) $ length nSegmentArgs
    where
        inverseExpr = case expr of
            Expr.Accessor accName _ -> Accessor accName
            Expr.Cons conName       -> Cons conName
            Expr.Var  variable      -> Var variable


fromSegment :: Pattern.Segment base arg -> MultiPartSegment base
fromSegment (Pattern.Segment nBase nSegmentArgs) =
    MultiPartSegment nBase $ length nSegmentArgs


toNamePat :: MultiPartExpr a -> State ASTInfo (LExpr a V) -> [AppArg a V]
          -> State ASTInfo (AppArg a V)
          -> State ASTInfo (Pattern.NamePat (LExpr a V) (AppArg a V))
toNamePat (MultiPartExpr mPrefix mBase mSegments) self args missing =
    flip evalStateT (map return args ++ repeat missing) $ do
        pPrefix   <- if mPrefix then Just <$> arg else return Nothing
        pBase     <- processBase self mBase
        pSegments <- mapM processSegments mSegments
        return $ Pattern.NamePat pPrefix pBase pSegments
    where
        processSegments :: MultiPartSegment Pattern.SegmentName
                        -> StateT [State ASTInfo (AppArg a V)] (State ASTInfo)
                                  (Pattern.Segment Pattern.SegmentName (AppArg a V))
        processSegments  (MultiPartSegment mBase' mCount) = Pattern.Segment mBase' <$> replicateM mCount arg

        processBase :: State ASTInfo (LExpr a V) -> MultiPartSegment (a, InverseExpr)
                    -> StateT [State ASTInfo (AppArg a V)] (State ASTInfo)
                              (Pattern.Segment (LExpr a V) (AppArg a V))
        processBase self (MultiPartSegment (label, inverseExpr) mCount) = do
            expr <- case inverseExpr of
                Accessor accName  -> Label label . Expr.Accessor accName <$> lift self
                Cons     conName  -> return $ Label label $ Expr.Cons conName
                Var      variable -> return $ Label label $ Expr.Var variable
            Pattern.Segment expr <$> replicateM mCount arg

        arg :: StateT [State ASTInfo (AppArg a V)] (State ASTInfo) (AppArg a V)
        arg = do
            h:t <- get
            put t
            lift h

getName :: MultiPartExpr a -> String
getName (MultiPartExpr _ base' segments') = unwords $ getName' base' : map getName'' segments' where
    getName'  (MultiPartSegment (_, Accessor n) _) = lunaShow n
    getName'  (MultiPartSegment (_, Cons     n) _) = toString n
    getName'  (MultiPartSegment (_, Var      v) _) = lunaShow v
    getName'' (MultiPartSegment sn              _) = toString sn
