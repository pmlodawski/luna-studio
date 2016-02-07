{-# LANGUAGE UndecidableInstances #-}

module Luna.Syntax.Model.Graph.Edge where

import Prelude.Luna

import Luna.Syntax.Model.Graph.Class
import Luna.Syntax.Model.Graph.Ref
import Luna.Syntax.Model.Graph.Node

import Data.Prop
import Control.Monad.Event
import Data.Direction
import Data.Index
import Data.Container          hiding (Impossible)
import Luna.Evaluation.Runtime as Runtime
import Type.Bool


------------------
-- === Edge === --
------------------

-- === Definitions === --

data Edge src tgt = Edge (Ref src) (Ref tgt) deriving (Show, Eq, Ord)
type Link       a = Edge a a


-- === Utils === --

edge :: Ref (Node src) -> Ref (Node tgt) -> Edge src tgt
edge src tgt = Edge (rewrap src) (rewrap tgt)

source :: Lens' (Edge src tgt) (Ref (Node src))
source = lens (\(Edge src _) -> rewrap src) (\(Edge _ tgt) src -> Edge (rewrap src) tgt)

target :: Lens' (Edge src tgt) (Ref (Node tgt))
target = lens (\(Edge _ tgt) -> rewrap tgt) (\(Edge src _) tgt -> Edge src (rewrap tgt))


-- === Instances === --

-- Primitive

type instance Target (Ref (Edge src tgt)) = Ref (Node tgt)
type instance Source (Ref (Edge src tgt)) = Ref (Node src)

-- Conversions

instance (Castable (Ref src) (Ref src'), Castable (Ref tgt) (Ref tgt')) => Castable (Edge src tgt) (Edge src' tgt') where
    cast (Edge src tgt) = Edge (cast src) (cast tgt) ; {-# INLINE cast #-}

-- Properties

instance Castable e (Edge src tgt) => Getter (Ref (Edge src tgt)) (Graph n e) where getter ref     = cast ∘ index_ (ref ^. idx) ∘ view edgeGraph                    ; {-# INLINE getter #-}
instance Castable (Edge src tgt) e => Setter (Ref (Edge src tgt)) (Graph n e) where setter ref val = edgeGraph %~ unchecked inplace insert_ (ref ^. idx) (cast val) ; {-# INLINE setter #-}



------------------------
-- === Connection === --
------------------------

-- === Definitions === --

type family Connection     src tgt
type family NameConnection src tgt where
    NameConnection src I   = Impossible
    NameConnection src tgt = If (Runtime.Model tgt == Static) src (Connection src tgt)


type ConCtx src tgt conn = ( conn ~ Connection src tgt
                           , src  ~ Source conn
                           , tgt  ~ Target conn
                           )

type Linkable        t       m = Connectible      t   t   m
type Connectible     src tgt m = Connectible'     src tgt m (Connection     src tgt)
type ConnectibleName src tgt m = ConnectibleName' src tgt m (NameConnection src tgt)

class ConCtx src tgt conn => Connectible'        src tgt m conn |    src tgt -> conn, conn     -> src tgt where connection      ::             src -> tgt -> m conn
class                        ConnectibleName'    src tgt m conn |    src tgt -> conn, conn tgt -> src     where nameConnection  ::             src -> tgt -> m conn
class                        ConnectibleNameH rt src tgt m conn | rt src tgt -> conn, conn rt  -> src     where nameConnectionH :: Proxy rt -> src -> tgt -> m conn


-- === Instances === --

type instance Connection (Ref  a) (Ref  b) = Ref (Connection a b)
type instance Connection (Node a) (Node b) = Edge a b

instance (LayerConstructor m c, Dispatcher CONNECTION c m, Unlayered c ~ Edge src tgt, c ~ Connection (Ref (Node src)) (Ref (Node tgt)))
      => Connectible' (Ref (Node src)) (Ref (Node tgt)) m c where
         connection src tgt = dispatch CONNECTION =<< constructLayer (edge src tgt)

instance (ConnectibleNameH mod src tgt m conn
         , mod ~ Runtime.Model tgt)  => ConnectibleName'         src tgt m  conn where nameConnection          = nameConnectionH (Proxy :: Proxy mod) ; {-# INLINE nameConnection  #-}
instance (Monad m, conn ~ src)       => ConnectibleNameH Static  src tgt m  conn where nameConnectionH _ src _ = return src                           ; {-# INLINE nameConnectionH #-}
instance Connectible' src tgt m conn => ConnectibleNameH Dynamic src tgt m  conn where nameConnectionH _       = connection                           ; {-# INLINE nameConnectionH #-}
