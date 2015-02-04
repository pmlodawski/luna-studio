module Luna.Typechecker.AlphaEquivSpec (spec) where


import Luna.Typechecker.AlphaEquiv
import Luna.Typechecker.Data.TVar
import Luna.Typechecker.Data.Type
import Test.Hspec.LunaTypechecker



spec :: Spec
spec = do
  describe "instance AlphaEquiv Type" $ do
    it "positively compares two equal types" $ example $ True `isResultOf` runEquiv (TV$TVar 1)                     (TV$TVar 1)




--ts :: [(Bool, Type, Type)]
--ts=[ (True,  (TV$TVar 1),                     (TV$TVar 1)                    )
--   , (True,  (TV$TVar 2),                     (TV$TVar 1)                    )


--   , (True,  ((TV$TVar 1) `Fun` (TV$TVar 1)), ((TV$TVar 1) `Fun` (TV$TVar 1)))
--   , (False, ((TV$TVar 1) `Fun` (TV$TVar 1)), ((TV$TVar 1) `Fun` (TV$TVar 0)))
--   , (False, ((TV$TVar 1) `Fun` (TV$TVar 1)), ((TV$TVar 0) `Fun` (TV$TVar 1)))
--   , (True,  ((TV$TVar 1) `Fun` (TV$TVar 1)), ((TV$TVar 0) `Fun` (TV$TVar 0)))

--   , (False, ((TV$TVar 1) `Fun` (TV$TVar 0)), ((TV$TVar 1) `Fun` (TV$TVar 1)))
--   , (True,  ((TV$TVar 1) `Fun` (TV$TVar 0)), ((TV$TVar 1) `Fun` (TV$TVar 0)))
--   , (True,  ((TV$TVar 1) `Fun` (TV$TVar 0)), ((TV$TVar 0) `Fun` (TV$TVar 1)))
--   , (False, ((TV$TVar 1) `Fun` (TV$TVar 0)), ((TV$TVar 0) `Fun` (TV$TVar 0)))

--   , (False, ((TV$TVar 0) `Fun` (TV$TVar 1)), ((TV$TVar 1) `Fun` (TV$TVar 1)))
--   , (True,  ((TV$TVar 0) `Fun` (TV$TVar 1)), ((TV$TVar 1) `Fun` (TV$TVar 0)))
--   , (True,  ((TV$TVar 0) `Fun` (TV$TVar 1)), ((TV$TVar 0) `Fun` (TV$TVar 1)))
--   , (False, ((TV$TVar 0) `Fun` (TV$TVar 1)), ((TV$TVar 0) `Fun` (TV$TVar 0)))

--   , (True,  ((TV$TVar 0) `Fun` (TV$TVar 0)), ((TV$TVar 1) `Fun` (TV$TVar 1)))
--   , (False, ((TV$TVar 0) `Fun` (TV$TVar 0)), ((TV$TVar 1) `Fun` (TV$TVar 0)))
--   , (False, ((TV$TVar 0) `Fun` (TV$TVar 0)), ((TV$TVar 0) `Fun` (TV$TVar 1)))
--   , (True,  ((TV$TVar 0) `Fun` (TV$TVar 0)), ((TV$TVar 0) `Fun` (TV$TVar 0)))


--   , (True,  ((TV$TVar 8) `Fun` (TV$TVar 8)), ((TV$TVar 1) `Fun` (TV$TVar 1)))
--   , (False, ((TV$TVar 8) `Fun` (TV$TVar 8)), ((TV$TVar 1) `Fun` (TV$TVar 0)))
--   , (False, ((TV$TVar 8) `Fun` (TV$TVar 8)), ((TV$TVar 0) `Fun` (TV$TVar 1)))
--   , (True,  ((TV$TVar 8) `Fun` (TV$TVar 8)), ((TV$TVar 0) `Fun` (TV$TVar 0)))

--   , (False, ((TV$TVar 8) `Fun` (TV$TVar 9)), ((TV$TVar 1) `Fun` (TV$TVar 1)))
--   , (True,  ((TV$TVar 8) `Fun` (TV$TVar 9)), ((TV$TVar 1) `Fun` (TV$TVar 0)))
--   , (True,  ((TV$TVar 8) `Fun` (TV$TVar 9)), ((TV$TVar 0) `Fun` (TV$TVar 1)))
--   , (False, ((TV$TVar 8) `Fun` (TV$TVar 9)), ((TV$TVar 0) `Fun` (TV$TVar 0)))

--   , (False, ((TV$TVar 9) `Fun` (TV$TVar 8)), ((TV$TVar 1) `Fun` (TV$TVar 1)))
--   , (True,  ((TV$TVar 9) `Fun` (TV$TVar 8)), ((TV$TVar 1) `Fun` (TV$TVar 0)))
--   , (True,  ((TV$TVar 9) `Fun` (TV$TVar 8)), ((TV$TVar 0) `Fun` (TV$TVar 1)))
--   , (False, ((TV$TVar 9) `Fun` (TV$TVar 8)), ((TV$TVar 0) `Fun` (TV$TVar 0)))

--   , (True,  ((TV$TVar 9) `Fun` (TV$TVar 9)), ((TV$TVar 1) `Fun` (TV$TVar 1)))
--   , (False, ((TV$TVar 9) `Fun` (TV$TVar 9)), ((TV$TVar 1) `Fun` (TV$TVar 0)))
--   , (False, ((TV$TVar 9) `Fun` (TV$TVar 9)), ((TV$TVar 0) `Fun` (TV$TVar 1)))
--   , (True,  ((TV$TVar 9) `Fun` (TV$TVar 9)), ((TV$TVar 0) `Fun` (TV$TVar 0)))

--   , (True,   Record [("a",TV$TVar 0), ("b",TV$TVar 1), ("c",TV$TVar 2)],
--              Record [("x",TV$TVar 7), ("y",TV$TVar 8), ("z",TV$TVar 9)])
--   , (True,   Record [("a",TV$TVar 0), ("b",TV$TVar 1), ("c",TV$TVar 2)],
--              Record [("a",TV$TVar 0), ("b",TV$TVar 1), ("c",TV$TVar 2)])
--   , (False,  Record [("a",TV$TVar 0), ("b",TV$TVar 1), ("c",TV$TVar 2)],
--              Record [("x",TV$TVar 7)])
--   , (False,  Record [("a",TV$TVar 0), ("b",TV$TVar 1), ("c",TV$TVar 2)],
--              Record [])

--   , (True,   Record [("a",((TV$TVar 8) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 2)],
--              Record [("x",((TV$TVar 1) `Fun` (TV$TVar 0))), ("y",TV$TVar 8), ("z",TV$TVar 9)])
   
--   , (True,   (TV$TVar 8) `Fun` (Record [("a",((TV$TVar 8) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 2)]),
--              (TV$TVar 1) `Fun` (Record [("x",((TV$TVar 1) `Fun` (TV$TVar 0))), ("y",TV$TVar 8), ("z",TV$TVar 9)]))

--   , (True,   (TV$TVar 8) `Fun` (Record [("a",((TV$TVar 8) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 2)]) `Fun` (TV$TVar 9),
--              (TV$TVar 1) `Fun` (Record [("x",((TV$TVar 1) `Fun` (TV$TVar 0))), ("y",TV$TVar 8), ("z",TV$TVar 9)]) `Fun` (TV$TVar 0))


--   , (False,  Record [("a",((TV$TVar 8) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 8)],
--              Record [("x",((TV$TVar 1) `Fun` (TV$TVar 0))), ("y",TV$TVar 8), ("z",TV$TVar 9)])
   
--   , (False,  (TV$TVar 8) `Fun` (Record [("a",((TV$TVar 8) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 1)]),
--              (TV$TVar 1) `Fun` (Record [("x",((TV$TVar 1) `Fun` (TV$TVar 0))), ("y",TV$TVar 8), ("z",TV$TVar 9)]))

--   , (False,  (TV$TVar 8) `Fun` (Record [("a",((TV$TVar 1) `Fun` (TV$TVar 9))), ("b",TV$TVar 1), ("c",TV$TVar 2)]) `Fun` (TV$TVar 9),
--              (TV$TVar 1) `Fun` (Record [("x",((TV$TVar 1) `Fun` (TV$TVar 0))), ("y",TV$TVar 8), ("z",TV$TVar 9)]) `Fun` (TV$TVar 0))

--   , (True,   (TV$TVar 0) `Fun` (Record [("foo", (TV$TVar 0) `Fun` (TV$TVar 1))]) `Fun` (TV$TVar 1),
--              (TV$TVar 0) `Fun` (Record [("foo", (TV$TVar 0) `Fun` (TV$TVar 1))]) `Fun` (TV$TVar 1))
--   ]