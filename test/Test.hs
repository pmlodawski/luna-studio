import Prelude


type Name = String
type Type = Int

newtype Mu f = Mu (f (Mu f))

newtype MuH f h = MuH (f (h (MuH f h)))

-------------------------------------------------------------

--data Expr e = Cons     Name
--            | Accessor Name e
--            | App      e e

--data Stat e = Var  Name
--            | Expr (Expr e)


--data Node h = Val  (MuH Expr h)
--            | Stat (Stat (h (Node h)))


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

data Expr e h = Cons     Name
              | Accessor Name (h (e h))
              | App      (h (e h)) (h (e h))

data Stat e h = Var  Name
              | Expr (Expr e h)


data MuX e h = MuX (e (e (MuX e h))) ...
-- TODO
data Node h = Val  (Expr h)
            -- | Stat (Stat (h (Node h)))



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
