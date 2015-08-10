import Prelude


type Name = String
type Type = Int

newtype Mu f = Mu (f (Mu f))

newtype MuH f h = MuH (f (h (MuH f h)))

--data Arg a = Arg a

-------------------------------------------------------------

--data Val   a = Lit
--             | Cons     Name [a]

--data Const a = Accessor Name a
--             | App      a a
--             | Val      (Val a)

--data Term  a = Var      Name
--             | Const    (Const a)



--nizej datatypy sa zle - NodeTerm nie uwzglednia rekurencyjnych node termow

--data NodeConstTerm = Value    (Mu Val)
--                   | Constant (Const NodeConstTerm)

--data NodeTerm = NodeConstTerm NodeConstTerm
--              | Expr     (Term NodeTerm)


--v1 = Constant $ App (Constant (Val Lit)) (Constant (Val Lit)) :: NodeConstTerm
--v2 = App (NodeConstTerm (Constant (Val Lit))) (Expr (Var "foo")) :: _

data Lit = Int Int
         | String String
         deriving (Show)

newtype Var        = Var      Name      deriving (Show, Eq)
data    Cons     a = Cons     Name [a]  deriving (Show)
data    Accessor a = Accessor Name a    deriving (Show)
data    App      a = App      a [Arg a] deriving (Show)



data Val = LitV Lit
         | ConsV (Cons Val)
         deriving (Show)

data ConstTerm = CLit Lit
               | CCons
--v1 = (Constant (Val Lit))
--v2 = Const $ App (Const (Val Lit)) (Var "Foo")

--data LabelH l a = LabelH l (a (LabelH l a))

--data Label l a = Label l a
--data TCVal   a = TCVal Type a

--type Foo1 l = Stat (LabelH l Stat)

--type LStat l = MuH Stat (Label l)
--type TCStat  = MuH Stat TCVal
--type TCNode  = Node TCVal

-------------------------------------------------------------

-- tak nie da sie zdeklarowac

--data Expr h = Cons     Name
--            | Accessor Name (h Expr)
--            | App      (h Expr) (h Expr)

-------------------------------------------------------------

--data Expr e h = Cons     Name
--              | Accessor Name (h (e h))
--              | App      (h (e h)) (h (e h))

--data Stat e h = Var  Name
--              | Expr (Expr e h)


--data MuX e h = MuX (e (e (MuX e h))) ...
---- TODO
--data Node h = Val  (Expr h)
--            -- | Stat (Stat (h (Node h)))



-------------------------------------------------------------


--data Expr h = Cons     Name
--            | Accessor Name (h (Expr h))
--            | App      (h (Expr h)) (h (Expr h))

---- nie da sie zrobic stat z analogiczna sygnatura!
----data Stat h = Var  Name
----            | Expr (Expr h) -- potrzebne jest polaczenie Stat i h

------ ponizszy sposob tez nie jest mozliwy
------ daloby sie zrobic zagniezdzone klasy wrapujace, ale jest to nieczytelne
----data Stat e = Var Name
----            | Expr e

----data Node h = Val  (Expr h)
----            | Stat (Stat (Expr ...) -- znow potrzeba polaczyc Stat i h
----                                    -- Stat (Expr (h (...))) - tylko ze nie zgadzaja sie kindy

--data X h a = X (h (Stat a))

--data Stat e = Var Name
--            | Expr e

--type Foo h = Expr (X h)
