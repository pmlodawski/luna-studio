
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}


module Main where

import Flowbox.Prelude hiding (simple, empty)
import Data.Repr

--import qualified Luna.Inference.Type as Type
import           Luna.Inference.Type
import qualified Data.Map            as Map
import           Data.Map            (Map)
import qualified Data.Text.Lazy      as Text
import           Data.Text.Lazy      (Text)

import           Luna.Inference.RawData

import           Luna.Inference.Function

import           GHC.Prim (Any)
import           Unsafe.Coerce (unsafeCoerce)
import           Data.Convert

import qualified Data.Graph.Inductive as Graph
import           FastString (FastString, mkFastString)


import qualified Data.IntMap.Lazy as IntMap
import           Data.IntMap.Lazy (IntMap)
import Data.Typeable
import Control.Monad.State


add :: Int -> Int -> Int
add = (+)

add2 :: (Int, (Int, ())) -> Int
add2 (a, (b, ())) = a + b


testf :: (Int, (String, ())) -> (Int,String)
testf (a,(b,())) = (a,b)

--unpackTest :: Object -> (Int,String)
--unpackTest = unpackRawData


--unpackInt :: Object -> Int
--unpackInt = unpackRawData

f = unsafeCoerce add :: Any

appx :: a -> Any -> Any
appx a f = (unsafeCoerce f :: Any -> Any) (unsafeCoerce a :: Any)

instance Default (Graph.Gr a b) where
    def = Graph.mkGraph def def

empty = def :: Graph.Gr Node ()


instance IsString FastString where
    fromString = mkFastString

type ID = Int

--data Expr = Input    FastString
--          | Accessor FastString
--          | Cons     FastString
--          | App
--          deriving (Show)




type MonadGraphBuilder m = MonadState GraphBuilder m

type Name = String

data Expr = Var      { _name :: Name                                                      }
          | Cons     { _name :: Name                                                      }
          | Accessor { _name :: Name, _base :: Connection Expr                            }
          | App      { _name :: Name, _base :: Connection Expr, _args :: [Arg] }
          deriving (Show)



class MonadGraphBuilder m => Accessible m a where
    access :: Name -> a -> m Expr

--instance MonadGraphBuilder Expr where
--    access n = Accessor n

--instance Accessible Expr where
--    access =




data Arg = Arg { _label :: Maybe Name, _val :: Connection Expr }
         deriving (Show)


data Node = Node { _tp   :: ID
                 , _expr :: Expr
                 , _out  :: [Connection Expr]
                 } deriving (Show)


class IsExpr a where
    toExpr :: a -> Expr

--inputs = [0]

--g = empty
--  & Graph.insNode (0, Node 0 $ Input    "a"  )
--  & Graph.insNode (1, Node 1 $ Accessor "foo")
--  & Graph.insNode (2, Node 2 $ App)
--  & Graph.insNode (3, Node 3 $ Cons "Main")
--  & Graph.insNode (4, Node 4 $ Accessor "bar")
--  & Graph.insNode (5, Node 5 $ App)

type Connection t = Key t
newtype Key t = Key Int

instance Typeable t => Show (Key t) where
    show (Key i) = "Key " <> show (typeOf (undefined :: t)) <> " " <> show i



newtype Graph = Graph (IntMap Hidden) deriving (Monoid, Show)


instance Default Graph where
    def = Graph def


data Hidden where
    Hidden :: (IsExpr a, Show a) => a -> Hidden

instance Show Hidden where show (Hidden a) = show a


fromHidden :: Hidden -> a
fromHidden (Hidden a) = unsafeCoerce a

unhideExpr :: Hidden -> Expr
unhideExpr (Hidden a) = toExpr a


insert :: Key a -> a -> Graph -> Graph
insert (Key i) k (Graph g) = Graph $ IntMap.insert i (unsafeCoerce k) g

insertAt :: Int -> k -> Graph -> Graph
insertAt = insert . Key

lookup :: Key k -> Graph -> Maybe k
lookup (Key i) (Graph g) = fromHidden <$> IntMap.lookup i g

lookupAt :: Int -> Graph -> Maybe Expr
lookupAt i (Graph g) = unhideExpr <$> IntMap.lookup i g

--graph = mempty
--      & insertAt 0 (Input "a")




data GraphBuilder = GraphBuilder { _lastConnection :: Int
                                 , _lastNode       :: Int
                                 , _graph          :: Graph
                                 }

instance Default GraphBuilder where
    def = GraphBuilder def def def



makeLenses ''GraphBuilder

getBuilder :: MonadGraphBuilder m => m GraphBuilder
getBuilder = get

putBuilder :: MonadGraphBuilder m => GraphBuilder -> m ()
putBuilder = put

newConnection :: MonadGraphBuilder m => m (Connection a)
newConnection = do
    bldr <- getBuilder
    let i = bldr ^. lastConnection
    put $ bldr & lastConnection .~ (i + 1)
    return $ Key i


newNodeKey :: MonadGraphBuilder m => m (Key a)
newNodeKey = do
    bldr <- getBuilder
    let i = bldr ^. lastNode
    put $ bldr & lastNode .~ (i + 1)
    return $ Key i

addNode :: MonadGraphBuilder m => Key a -> a -> m ()
addNode k a = withBuilder $ graph %~ insert k a

mkNode :: MonadGraphBuilder m => a -> m (Key a)
mkNode a = do
    key <- newNodeKey
    addNode key a
    return key

withBuilder :: MonadGraphBuilder m => (GraphBuilder -> GraphBuilder) -> m ()
withBuilder f = do
    bldr <- getBuilder
    putBuilder $ f bldr

var :: MonadGraphBuilder m => Name -> m (Key Expr)
var n = mkNode $ Var n

cons :: MonadGraphBuilder m => Name -> m (Key Expr)
cons n = mkNode $ Var n

g1 :: Graph
g1 = view graph $ flip execState def $ do
    a <- var "a"
    b <- var "b"
    return $ a


main = do
    --let a = unsafeCoerce (return (1:: Int)) :: Monad m => m Int
        --b = unsafeCoerce a :: [Int]
        --a = mkObject (1 :: Int)
        --b = mkObject (2 :: Int)
        --s = mkObject ("ala" :: String)
        --c = appSimple add_Int [a,b]
        --c = appSimple (toDataFunc add2) [a,b]
        --x = appSimple (toDataFunc testf) [a,s]


    --print $ unpackInt a
    --print $ unpackInt b
    --print $ unpackInt c
    --print $ unpackTest x
    --let ss = fmap (fromString . show) [1..10000] :: [FastString]
    --    a  = (==) <$> ss <*> ss
    --    b  = length $ filter (==True) a

    --print $ b

    print $ (unsafeCoerce $ appx (1 :: Int) $ appx (2 :: Int) f :: Int)
    print g1
    print "end"

    --print graph
    --print b
    --print g

