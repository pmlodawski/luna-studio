---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Data.Channel where

import Data.Map as Map

import           Flowbox.Prelude
import Debug.Trace as Dbg



data ChannelTree name value = ChannelTree  { _channel  :: Maybe value
                                           , _children :: Map name (ChannelTree name value)
                                           }
                            | EmptyNode
                            deriving (Show)
makeLenses ''ChannelTree

data Crumb name value = Crumb name (Maybe value) (Map name (ChannelTree name value)) deriving (Show)
type Breadcrumbs name value = [Crumb name value]
type Zipper name value = (ChannelTree name value, Breadcrumbs name value)
type ZipperResult name value = Either ZipperError (Zipper name value)

data ZipperError = UnreachableError
                 | UpUnreachableError
                 | AttachToEmpty
                 | AppendToEmpty
                 | InsertToExisting
                 | AlterEmptyError
                 | RemoveEmptyError
                 | GetNonExistant
                 deriving (Show, Eq)

-- == Tree ==

empty :: Ord name => ChannelTree name value
empty = ChannelTree Nothing mempty

-- == Traversing ==

zipper :: ChannelTree name value -> ZipperResult name value
zipper t = return (t, [])

tree :: Zipper name value -> ChannelTree name value
tree (t, _) = t

lookup :: (Show name , Ord name) => name -> Zipper name value -> ZipperResult name value
lookup _ (EmptyNode, _) = Dbg.trace "UnreachableError" $ Left UnreachableError
lookup name (ChannelTree chan tree, bs) = Dbg.trace ("lookup: " ++ show name) $ case Map.lookup name tree of
    Nothing    -> Left UnreachableError
    Just tree' -> Right (tree', Crumb name chan rest:bs)
    where rest = Map.delete name tree

up :: Ord name => Zipper name value -> ZipperResult name value
up (_, []) = Left UpUnreachableError
up (t, Crumb name chan rest:bs) = case t of
    EmptyNode -> Right (ChannelTree chan rest, bs)
    node      -> Right (ChannelTree chan (Map.insert name node rest), bs)

top :: Ord name => Zipper name value -> ZipperResult name value
top z = case up z of
    Left  _ -> Right z
    Right u -> top u

-- == Modifications ==

-- = Tree =

attach :: Ord name => name -> ChannelTree name value -> Zipper name value -> ZipperResult name value
attach name new (t, bs) = case t of
    EmptyNode             -> Left AttachToEmpty
    ChannelTree chan tree -> Right (ChannelTree chan (Map.insert name new tree), bs)

insert :: Ord name => Maybe value -> Zipper name value -> ZipperResult name value
insert v (t, bs) = case t of
    EmptyNode -> Right (ChannelTree v mempty, bs)
    _         -> Left InsertToExisting

append :: Ord name => name -> Maybe value -> Zipper name value -> ZipperResult name value
append name v = attach name (ChannelTree v mempty)

delete :: Zipper name value -> ZipperResult name value
delete (t, bs) = case t of
    EmptyNode       -> Left RemoveEmptyError
    ChannelTree _ _ -> Right (EmptyNode, bs)

-- = Value =

clear :: Zipper name value -> ZipperResult name value
clear (t, bs) = case t of
    EmptyNode          -> Left AlterEmptyError
    ChannelTree _ tree -> Right (ChannelTree Nothing tree, bs)

set :: value -> Zipper name value -> ZipperResult name value
set v (t, bs) = case t of
    EmptyNode          -> Left AlterEmptyError
    ChannelTree _ tree -> Right (ChannelTree (Just v) tree, bs)

modify :: (value -> value) -> Zipper name value -> ZipperResult name value
modify f (t, bs) = case t of
    EmptyNode             -> Left AlterEmptyError
    ChannelTree chan tree -> Right (ChannelTree (fmap f chan) tree, bs)

get :: Zipper name value -> Either ZipperError (Maybe value)
get (EmptyNode, _)         = Left GetNonExistant
get (ChannelTree val _, _) = Right val
