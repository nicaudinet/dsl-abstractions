module Main where

import Prelude hiding (LT)
import Data.List (intercalate)
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, frequency)
import Control.Monad.Omega (Omega, runOmega)
import Data.Foldable (asum)

main :: IO ()
main = do
  part1
  part2

part1 :: IO ()
part1 = do
  putStrLn ""
  putStrLn "#######"
  putStrLn "Part 1:"
  putStrLn "#######"
  putStrLn ""

  putStrLn "The functions:"
  mapM_ (putStrLn . (" - " <>) . render) functions
  putStrLn ""

  putStrLn "Tests:"
  mapM_ quickCheck tests

part2 :: IO ()
part2 = do
  putStrLn ""
  putStrLn "#######"
  putStrLn "Part 2:"
  putStrLn "#######"
  putStrLn ""

  putStrLn "Program:\tLog Likelihood:"
  -- mapM_ (putStrLn . renderProb) (take 50 (enumerate 2))
  mapM_ (putStrLn . render) (take 20 (runOmega programs))
  putStrLn ""

  putStrLn "To Find:\t\tEpsilon:\tFound:"
  findAndPrint "f(x) = x * 2:\t\t" 0 (\x -> x * 2)
  findAndPrint "f(x) = abs(x):\t\t" 0.001 (\x -> abs x)
  findAndPrint "f(x) = x + 0.05:\t" 0.1 (\x -> x + 0.05)
  findAndPrint "f(x) = x ** 2:\t\t" 0.1 (\x -> x ** 2)
  findAndPrint "f(x) = (x ** 2) + 1:\t" 0.1 (\x -> x ** 2 + 1)
  findAndPrint "f(x) = cos(x) ** 2:\t" 0.001 (\x -> (cos x) ** 2)

findAndPrint :: String -> Float -> (Float -> Float) -> IO ()
findAndPrint title epsilon f =
  putStrLn (title <> show epsilon <> "\t\t" <> render (findProgram epsilon f))

------------
-- Part 1 --
------------

data Expr
  = Zero
  | One
  | Two
  | Input
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Exp Expr Expr
  | LT Expr Expr
  | Sqrt Expr
  | Sin Expr
  | ITE Expr Expr Expr

instance Arbitrary Expr where
  arbitrary = frequency $ zip ([1,1,1,1] <> (repeat 0))
    [ pure Zero
    , pure One
    , pure Two
    , pure Input
    , Add <$> arbitrary <*> arbitrary
    , Sub <$> arbitrary <*> arbitrary
    , Mul <$> arbitrary <*> arbitrary
    , Div <$> arbitrary <*> arbitrary
    , Exp <$> arbitrary <*> arbitrary
    , LT <$> arbitrary <*> arbitrary
    , Sqrt <$> arbitrary
    , Sin <$> arbitrary
    , ITE <$> arbitrary <*> arbitrary <*> arbitrary
    ]

bracketed :: String -> String
bracketed str = "(" <> str <> ")"
renderBinOp :: String -> Expr -> Expr -> String
renderBinOp op e1 e2 = bracketed $ render e1 <> " " <> op <> " " <> render e2

render :: Expr -> String
render Zero = "0"
render One = "1"
render Two = "2"
render Input = "x"
render (Add e1 e2) = renderBinOp "+" e1 e2
render (Sub e1 e2) = renderBinOp "-" e1 e2
render (Mul e1 e2) = renderBinOp "*" e1 e2
render (Div e1 e2) = renderBinOp "/" e1 e2
render (Exp e1 e2) = renderBinOp "^" e1 e2
render (LT e1 e2) = renderBinOp "<" e1 e2
render (Sqrt e) = "sqrt(" <> render e <> ")"
render (Sin e) = "sin(" <> render e <> ")"
render (ITE e1 e2 e3) = bracketed $ intercalate " "
  [ "if", render e1, "< 0"
  , "then", render e2
  , "else", render e3
  ]

eval :: Float -> Expr -> Float
eval _ Zero = 0
eval _ One = 1
eval _ Two = 2
eval i Input = i
eval i (Add e1 e2) = eval i e1 + eval i e2
eval i (Sub e1 e2) = eval i e1 - eval i e2
eval i (Mul e1 e2) = eval i e1 * eval i e2
eval i (Div e1 e2) = eval i e1 / eval i e2
eval i (Exp e1 e2) = eval i e1 ** eval i e2
eval i (LT e1 e2) = if eval i e1 < eval i e2 then 1 else -1
eval i (Sqrt e) = sqrt (eval i e)
eval i (Sin e) = sin (eval i e)
eval i (ITE e1 e2 e3) = if eval i e1 < 0 then eval i e2 else eval i e3

f0, f1, f2, f3 :: Expr
f0 = Add Input One
f1 = Add (Exp Input Two) (Div Input (Sin Input))
f2 = Exp (Add Input Two) Input
f3 = ITE Input (Exp Input Two) (Sqrt (Add (Exp Input Two) One))

functions :: [Expr]
functions = [f0, f1, f2, f3]

infix 4 ~=
(~=) :: Float -> Float -> Bool
a ~= b
  | isNaN a && isNaN b = True
  | isInfinite a && isInfinite b = True
  | otherwise = abs (a - b) < 0.0001 

tests :: [Float -> Bool]
tests =
  [ \x -> eval x f0 ~= x + 1
  , \x -> eval x f1 ~= (x ** 2) + (x / sin x)
  , \x -> eval x f2 ~= (x + 2) ** x
  , \x -> eval x f3 ~= if x < 0 then x**2 else sqrt (x**2 + 1)
  ]

------------
-- Part 2 --
------------

-- Functions to group types of nodes together

-- Another potential approach
-- https://stackoverflow.com/questions/28100650/generate-all-possible-trees

-- https://stackoverflow.com/questions/23515191/how-to-enumerate-a-recursive-datatype-in-haskell/23517557#23517557
sizes :: Omega Expr
sizes = asum $
  [ pure Zero
  , Add <$> sizes <*> sizes
  ]

programs :: Omega Expr
programs = asum
  [ pure Zero
  , pure One
  , pure Two
  , pure Input
  , Sqrt <$> programs
  , Sin <$> programs
  , Add <$> programs <*> programs
  , Sub <$> programs <*> programs
  , Mul <$> programs <*> programs
  , Div <$> programs <*> programs
  , Exp <$> programs <*> programs
  , ITE <$> programs <*> programs <*> programs
  ]

testProgram :: Float -> Expr -> Float -> Float -> Bool
testProgram epsilon expr input output =
  abs (output - (eval input expr)) <= epsilon

inputs :: [Float]
inputs = [(4 * x / 49) - 2 | x <- [0 .. 49]]

testAll :: Float -> (Float -> Float) -> Expr -> Maybe Expr
testAll epsilon fn expr =
  if all (uncurry (testProgram epsilon expr)) (zip inputs (map fn inputs))
  then Just expr
  else Nothing

unfoldUntil :: (a -> Either a b) -> a -> b
unfoldUntil f a = either (unfoldUntil f) id (f a)

findProgram :: Float -> (Float -> Float) -> Expr 
findProgram epsilon fn = unfoldUntil go (runOmega programs) where
  go :: [Expr] -> Either [Expr] Expr
  go [] = error "infinite lists should never end..."
  go (p:ps) = maybe (Left ps) Right (testAll epsilon fn p)




-- A simple Applicative for probabilities

data Prob a = Prob a Float

instance Functor Prob where
  fmap f (Prob a like) = Prob (f a) like

instance Applicative Prob where
  pure a = Prob a 1
  (Prob f l1) <*> (Prob a l2) = Prob (f a) (l1 + l2)

renderProb :: Prob Expr -> String
renderProb (Prob expr like) = render expr <> "\t\t" <> show like

uniform :: a -> Prob a
uniform a = Prob a (log $ 1.0 / 12.0) -- number of possible statements from DSL

value :: Prob a -> a
value (Prob a _) = a

likelihood :: Prob a -> Float
likelihood (Prob _ l) = l

