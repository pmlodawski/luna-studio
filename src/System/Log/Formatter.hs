{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  System.Log.Formatter
-- Copyright   :  (C) 2015 Flowbox
-- License     :  Apache-2.0
-- Maintainer  :  Wojciech Daniło <wojciech.danilo@gmail.com>
-- Stability   :  stable
-- Portability :  portable
-----------------------------------------------------------------------------

module System.Log.Formatter where

import System.Log.Log   (Log)
import System.Log.Data  (Lvl(Lvl), Msg(Msg), LevelData(LevelData), readData, DataOf, Lookup)
import Text.PrettyPrint.ANSI.Leijen hiding ()

newtype Formatter a = Formatter { runFormatter :: Log a -> Doc }

instance Show (Formatter a) where
    show _ = "Formatter"


foo = colorLvlFormatter ("[" <:> Lvl <:> "] ") <:> Msg <:> " !"

colorLvlFormatter f = Formatter (\s -> let (LevelData pr _) = readData Lvl s in lvlColor pr $ runFormatter f s)

lvlColor lvl
    | lvl == 0  = id
    | lvl <= 2  = green
    | lvl == 3  = yellow
    | otherwise = red



mapFormatter f (Formatter a) = Formatter (f a)

(<:>) :: (FormatterBuilder a c, FormatterBuilder b c) => a -> b -> Formatter c
(<:>) a b = concatFormatters (buildFormatter a) (buildFormatter b)

concatFormatters :: Formatter a -> Formatter a -> Formatter a
concatFormatters (Formatter f) (Formatter g) = Formatter (\s -> f s <> g s)

class FormatterBuilder a b where
    buildFormatter :: a -> Formatter b

instance FormatterBuilder String a where
    buildFormatter a = Formatter $ const (text a)

instance FormatterBuilder Doc a where
    buildFormatter a = Formatter $ const a

--instance Lookup Lvl a => FormatterBuilder Lvl (Formatter a) where
--    buildFormatter a = Formatter $ pprint . readData a

instance (PPrint (DataOf seg), Lookup seg (Log a)) => FormatterBuilder seg a where
    buildFormatter a = Formatter $ pprint . readData a

instance (a~b) => FormatterBuilder (Formatter a) b where
    buildFormatter = id

class PPrint a where
    pprint :: a -> Doc

instance PPrint String where
    pprint = text

instance Pretty a => PPrint a where
    pprint = pretty

instance Pretty LevelData where
    pretty (LevelData _ name) = text name