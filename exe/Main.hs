module Main where

import Prelude hiding (LT)
import Data.List (intercalate, sortOn, unfoldr)
import Test.QuickCheck (Arbitrary, arbitrary, quickCheck, frequency)

main :: IO ()
main = do
  putStrLn "The functions:"
  mapM_ (putStrLn . (" - " <>) . render) functions
  putStrLn "\nTests:"
  mapM_ quickCheck tests
  putStrLn "\nEnumeration:"
  mapM_ (putStrLn . renderPL) (take 50 (enumerate 2))
  putStrLn "\nFind Programs:"
  putStrLn (render (findProgram 0 (\x -> x * 2)))
  putStrLn (render (findProgram 0.001 (\x -> abs x)))
  putStrLn (render (findProgram 0.1 (\x -> x + 0.05)))
  putStrLn (render (findProgram 0.1 (\x -> x ** 2)))
  putStrLn (render (findProgram 0.1 (\x -> x ** 2 + 1)))

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

renderBinOp :: String -> Expr -> Expr -> String
renderBinOp op e1 e2 = "(" <> render e1 <> " " <> op <> " " <> render e2 <> ")"

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
render (ITE e1 e2 e3) = intercalate " "
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

-- The Program Likelihood (PL) monad
data PL a = PL a Float

instance Functor PL where
  fmap f (PL a like) = PL (f a) like

instance Applicative PL where
  pure a = PL a 1
  (PL f l1) <*> (PL a l2) = PL (f a) (l1 + l2)

renderPL :: PL Expr -> String
renderPL (PL expr like) = render expr <> "\t" <> show like

nullaries :: [Expr]
nullaries = [Zero, One, Two, Input]

unaries :: [Expr -> Expr]
unaries = [Sqrt, Sin]

binaries :: [Expr -> Expr -> Expr]
binaries = [Add, Sub, Mul, Div, Exp, LT]

ternaries :: [Expr -> Expr -> Expr -> Expr]
ternaries = [ITE]

uniform :: a -> PL a
uniform a = PL a (log $ 1.0 / 12.0) -- number of possible statements from DSL

value :: PL a -> a
value (PL a _) = a

likelihood :: PL a -> Float
likelihood (PL _ l) = l

apply2 :: Applicative f => f (a -> b -> c) -> f a -> f b -> f c
apply2 f a b = f <*> a <*> b

apply3 :: Applicative f => f (a -> b -> c -> d) -> f a -> f b -> f c -> f d
apply3 f a b c = f <*> a <*> b <*> c

step :: [PL Expr] -> [PL Expr]
step children = sortOn likelihood programs
  where
    parents1 :: [PL Expr]
    parents1 = (map (<*>) (map uniform unaries)) <*> children

    parents2 :: [PL Expr]
    parents2 = (map apply2 (map uniform binaries)) <*> children <*> children

    parents3 :: [PL Expr]
    parents3
      = (map apply3 (map uniform ternaries))
      <*> children
      <*> children
      <*> children

    programs :: [PL Expr]
    programs = concat [children, parents1, parents2, parents3]

enumerate :: Int -> [PL Expr]
enumerate = reverse . sortOn likelihood . concat . unfoldr go . (,) (map uniform nullaries)
  where
    go :: ([PL Expr], Int) -> Maybe ([PL Expr], ([PL Expr], Int))
    go (children, n)
      | n <= 0 = Nothing
      | otherwise = Just (children, (step children, n - 1))

firstJust :: (a -> Maybe b) -> [a] -> Maybe b
firstJust _ [] = Nothing
firstJust f (x:xs) = maybe (firstJust f xs) Just (f x)

unfoldUntil :: (a -> Either a b) -> a -> b
unfoldUntil f a = either (unfoldUntil f) id (f a)

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

findProgram :: Float -> (Float -> Float) -> Expr
findProgram epsilon fn = unfoldUntil go 1
  where
    go :: Int -> Either Int Expr
    go n = case firstJust (testAll epsilon fn) (map value (enumerate n)) of
      Nothing -> Left (n+1)
      Just expr -> Right expr
