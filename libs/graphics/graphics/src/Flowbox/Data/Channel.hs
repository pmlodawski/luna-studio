---------------------------------------------------------------------------
-- Copyright (C) Flowbox, Inc - All Rights Reserved
-- Unauthorized copying of this file, via any medium is strictly prohibited
-- Proprietary and confidential
-- Flowbox Team <contact@flowbox.io>, 2014
---------------------------------------------------------------------------

module Flowbox.Data.Channel where

import Data.Map as Map

import           Flowbox.Prelude



data ChannelTree name value = ChannelTree  { _children :: Map name (ChannelTree name value)
                                           , _channel  :: Maybe value
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
empty = ChannelTree mempty Nothing

-- == Traversing ==

zipper :: ChannelTree name value -> ZipperResult name value
zipper t = return (t, [])

goTo :: Ord name => name -> Zipper name value -> ZipperResult name value
goTo name (ChannelTree tree chan, bs) = case Map.lookup name tree of
    Nothing    -> Left UnreachableError
    Just tree' -> Right (tree', Crumb name chan rest:bs)
    where rest = Map.delete name tree

up :: Ord name => Zipper name value -> ZipperResult name value
up (_, []) = Left UpUnreachableError
up (t, Crumb name chan rest:bs) = case t of
    EmptyNode -> Right (ChannelTree rest chan, bs)
    node      -> Right (ChannelTree (Map.insert name node rest) chan, bs)

top :: Ord name => Zipper name value -> ZipperResult name value
top z = case up z of
    Left  _ -> Right z
    Right u -> top u

-- == Modifications ==

-- = Tree =

attach :: Ord name => name -> ChannelTree name value -> Zipper name value -> ZipperResult name value
attach name new (t, bs) = case t of
    EmptyNode             -> Left AttachToEmpty
    ChannelTree tree chan -> Right (ChannelTree (Map.insert name new tree) chan, bs)

insert :: Ord name => Maybe value -> Zipper name value -> ZipperResult name value
insert v (t, bs) = case t of
    EmptyNode -> Right (ChannelTree mempty v, bs)
    _         -> Left InsertToExisting

append :: Ord name => name -> Maybe value -> Zipper name value -> ZipperResult name value
append name v = attach name (ChannelTree mempty v)

delete :: Zipper name value -> ZipperResult name value
delete (t, bs) = case t of
    EmptyNode       -> Left RemoveEmptyError
    ChannelTree _ _ -> Right (EmptyNode, bs)

-- = Value =

clear :: Zipper name value -> ZipperResult name value
clear (t, bs) = case t of
    EmptyNode          -> Left AlterEmptyError
    ChannelTree tree _ -> Right (ChannelTree tree $ Nothing, bs)

set :: value -> Zipper name value -> ZipperResult name value
set v (t, bs) = case t of
    EmptyNode          -> Left AlterEmptyError
    ChannelTree tree _ -> Right (ChannelTree tree $ Just v, bs)

modify :: (value -> value) -> Zipper name value -> ZipperResult name value
modify f (t, bs) = case t of
    EmptyNode             -> Left AlterEmptyError
    ChannelTree tree chan -> Right (ChannelTree tree $ fmap f chan, bs)

get :: ZipperResult name value -> Either ZipperError (Maybe value)
get (Left _) = Left GetNonExistant
get (Right (EmptyNode, _)) = Left GetNonExistant
get (Right (ChannelTree _ val, _)) = Right val
