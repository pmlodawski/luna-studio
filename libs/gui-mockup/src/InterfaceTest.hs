{-# LANGUAGE ForeignFunctionInterface #-}
module InterfaceTest where

adder :: Int -> Int -> IO Int  -- gratuitous use of IO
adder x y = return (x+y)
 
foreign export stdcall adder :: Int -> Int -> IO Int


--shift :: [(Int,Int)] -> IO [(Int,Int)]
--shift controlPoints = return $ map (\(x,y)->(x+10,y)) controlPoints

--shift :: [Int] -> IO [Int]
--shift controlPoints = return $ map (\x->x+10) controlPoints

--foreign export stdcall shift :: [Int] -> IO [Int]