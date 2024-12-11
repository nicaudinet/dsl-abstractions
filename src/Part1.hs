module Part1
  ( part1
  ) where

import DSL (Expr)
import qualified DSL as E
import Test.QuickCheck (quickCheck)

part1 :: IO ()
part1 = do
  putStrLn ""
  putStrLn "#######"
  putStrLn "Part 1:"
  putStrLn "#######"
  putStrLn ""

  putStrLn "The functions:"
  mapM_ (putStrLn . (" - " <>) . E.render) functions
  putStrLn ""

  putStrLn "Tests:"
  mapM_ quickCheck tests

f0, f1, f2, f3 :: Expr
f0 = E.add E.input E.one
f1 = E.add (E.exp E.input E.two) (E.div E.input (E.sin E.input))
f2 = E.exp (E.add E.input E.two) E.input
f3 = E.ite E.input (E.exp E.input E.two) (E.sqrt (E.add (E.exp E.input E.two) E.one))
functions :: [Expr]
functions = [f0, f1, f2, f3]

infix 4 ~=
(~=) :: Double -> Double -> Bool
a ~= b
  | isNaN a && isNaN b = True
  | isInfinite a && isInfinite b = True
  | otherwise = abs (a - b) < 0.0001 

tests :: [Double -> Bool]
tests =
  [ \x -> E.eval f0 x ~= x + 1
  , \x -> E.eval f1 x ~= (x ** 2) + (x / sin x)
  , \x -> E.eval f2 x ~= (x + 2) ** x
  , \x -> E.eval f3 x ~= if x < 0 then x**2 else sqrt (x**2 + 1)
  ]
