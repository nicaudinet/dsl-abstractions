module Main where

import Prelude hiding (LT)
import Data.List (intercalate, sortOn)
import System.Random.MWC.Distributions (standard)
import Test.QuickCheck (quickCheck)
import qualified Data.Map as M 
import qualified Control.Monad.WeightedSearch as W
import Control.Applicative (asum)

import DSL (Expr)
import qualified DSL as E

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

  putStrLn (pad 20 "Program:" <> "Likelihood:")
  mapM_ printWithProb (take 20 (W.toList enumerate))
  putStrLn ""
  
  putStrLn (pad 25 "To Find:" <> pad 10 "Epsilon:" <> "Found:")
  findAndPrint "f(x) = x * 2" 0 (\x -> x * 2)
  findAndPrint "f(x) = abs(x)" 0.001 (\x -> abs x)
  findAndPrint "f(x) = x + 0.05" 0.1 (\x -> x + 0.05)
  findAndPrint "f(x) = x ** 2" 0.1 (\x -> x ** 2)
  findAndPrint "f(x) = (x ** 2) + 1" 0.1 (\x -> x ** 2 + 1)
  findAndPrint "f(x) = cos(x) ** 2" 0.001 (\x -> (cos x) ** 2)
  findAndPrint "f(x) = cos(x)" 0.001 (\x -> cos x)

pad :: Int -> String -> String
pad n str = str <> replicate (n - length str) ' '

printWithProb :: Expr -> IO ()
printWithProb e = putStrLn (pad 20 (render e) <> show (log $ likelihood e))

findAndPrint :: String -> Float -> (Float -> Float) -> IO ()
findAndPrint title epsilon f = putStrLn $
  pad 25 title <> pad 10 (show epsilon) <> render (findProgram epsilon f)

------------
-- Part 1 --
------------

bracketed :: String -> String
bracketed str = "(" <> str <> ")"

renderBinOp :: String -> Expr -> Expr -> String
renderBinOp op e1 e2 = bracketed $ render e1 <> " " <> op <> " " <> render e2

render :: Expr -> String
render (E.Fix expr) = case expr of
  E.Zero -> "0"
  E.One -> "1"
  E.Two -> "2"
  E.Pi -> "pi"
  E.Input -> "x"
  E.Sqrt e -> "sqrt" <> bracketed (render e)
  E.Sin e -> "sin" <> bracketed (render e)
  E.Add e1 e2 -> renderBinOp "+" e1 e2
  E.Sub e1 e2 -> renderBinOp "-" e1 e2
  E.Mul e1 e2 -> renderBinOp "*" e1 e2
  E.Div e1 e2 -> renderBinOp "/" e1 e2
  E.Exp e1 e2 -> renderBinOp "^" e1 e2
  E.LT e1 e2 -> renderBinOp "<" e1 e2
  E.ITE e1 e2 e3 -> bracketed $ intercalate " "
    [ "if", render e1, "< 0"
    , "then", render e2
    , "else", render e3
    ]

size :: Expr -> Int
size (E.Fix expr) = case expr of
  E.Zero -> 1
  E.One -> 1
  E.Two -> 1
  E.Pi -> 1
  E.Input -> 1
  E.Sqrt e -> 1 + size e
  E.Sin e -> 1 + size e
  E.Add e1 e2 -> 1 + size e1 + size e2
  E.Sub e1 e2 -> 1 + size e1 + size e2
  E.Mul e1 e2 -> 1 + size e1 + size e2
  E.Div e1 e2 -> 1 + size e1 + size e2
  E.Exp e1 e2 -> 1 + size e1 + size e2
  E.LT e1 e2 -> 1 + size e1 + size e2
  E.ITE e1 e2 e3 -> 1 + size e1 + size e2 + size e3

likelihood :: Expr -> Float
likelihood expr = uniform ** (fromIntegral $ size expr)
  where
    uniform = 1 / 13 -- number of DSL nodes (without counting the input)

eval :: Expr -> Float -> Float
eval (E.Fix expr) i = case expr of
  E.Zero -> 0
  E.One -> 1
  E.Two -> 2
  E.Pi -> pi
  E.Input -> i
  E.Sqrt e -> sqrt (eval e i)
  E.Sin e -> sin (eval e i)
  E.Add e1 e2 -> eval e1 i + eval e2 i
  E.Sub e1 e2 -> eval e1 i - eval e2 i
  E.Mul e1 e2 -> eval e1 i * eval e2 i
  E.Div e1 e2 -> eval e1 i / eval e2 i
  E.Exp e1 e2 -> eval e1 i ** eval e2 i
  E.LT e1 e2 -> if eval e1 i < eval e2 i then 1 else -1
  E.ITE e1 e2 e3 -> if eval e1 i < 0 then eval e2 i else eval e3 i

f0, f1, f2, f3 :: Expr
f0 = E.add E.input E.one
f1 = E.add (E.exp E.input E.two) (E.div E.input (E.sin E.input))
f2 = E.exp (E.add E.input E.two) E.input
f3 = E.ite E.input (E.exp E.input E.two) (E.sqrt (E.add (E.exp E.input E.two) E.one))

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
  [ \x -> eval f0 x ~= x + 1
  , \x -> eval f1 x ~= (x ** 2) + (x / sin x)
  , \x -> eval f2 x ~= (x + 2) ** x
  , \x -> eval f3 x ~= if x < 0 then x**2 else sqrt (x**2 + 1)
  ]

------------
-- Part 2 --
------------

-- https://stackoverflow.com/questions/23515191/how-to-enumerate-a-recursive-datatype-in-haskell/23517557#23517557
-- https://web.archive.org/web/20140823135714/http://lukepalmer.wordpress.com/2008/05/02/enumerating-a-context-free-language/
enumerate :: W.T Integer Expr
enumerate = asum $ map (W.weight 1)
  [ pure E.zero
  , pure E.one
  , pure E.two
  , pure E.pi
  , pure E.input
  , E.sqrt <$> enumerate
  , E.sin <$> enumerate
  , E.add <$> enumerate <*> enumerate
  , E.sub <$> enumerate <*> enumerate
  , E.mul <$> enumerate <*> enumerate
  , E.div <$> enumerate <*> enumerate
  , E.exp <$> enumerate <*> enumerate
  , E.ite <$> enumerate <*> enumerate <*> enumerate
  ]

linspace :: Float -> Float -> Int -> [Float]
linspace start stop num = map scale [0 .. num]
  where
    scale :: Int -> Float
    scale i = start + (fromIntegral i) * (stop - start) / (fromIntegral num)

makeDataset :: (Float -> Float) -> Dataset
makeDataset fn =
  let ins = linspace (-2) 2 50
  in zip ins (map fn ins)

testProgram :: Float -> Expr -> Float -> Float -> Bool
testProgram epsilon expr input output =
  abs (output - (eval expr input)) <= epsilon

testAll :: Float -> (Float -> Float) -> Expr -> Maybe Expr
testAll epsilon fn expr =
  if all (uncurry (testProgram epsilon expr)) (makeDataset fn)
  then Just expr
  else Nothing

unfoldUntil :: (a -> Either a b) -> a -> b
unfoldUntil f a = either (unfoldUntil f) id (f a)

findProgram :: Float -> (Float -> Float) -> Expr 
findProgram epsilon fn = unfoldUntil go (W.toList enumerate) where
  go :: [Expr] -> Either [Expr] Expr
  go [] = error "infinite lists should never end..."
  go (p:ps) = maybe (Left ps) Right (testAll epsilon fn p)

-- Another potential approach:
-- https://stackoverflow.com/questions/28100650/generate-all-possible-trees

------------
-- Part 3 --
------------

numSequences :: Int
numSequences = 1000

lenSequences :: Int
lenSequences = 100

xs :: [Float]
xs = linspace (-10) 10 lenSequences

slack :: Int
slack = 20

stride :: Int
stride = 4

type Dataset = [(Float, Float)]

inputs :: Dataset -> [Float]
inputs = map fst

outputs :: Dataset -> [Float]
outputs = map snd

errors :: Dataset -> Expr -> [Float]
errors dataset expr =
  let progOutputs = map (eval expr) (inputs dataset)
  in zipWith (\x y -> (x - y) ** 2) progOutputs (outputs dataset)

argmin :: [Float] -> [Int]
argmin = map fst . sortOn snd . zip [0..]

values :: [[Float]]
values = undefined

-- bestFits :: Int -> [

type Tag = E.ExprF ()

data Bigram = Bigram Tag Tag
  deriving (Eq, Ord)

tag :: Expr -> Tag
tag (E.Fix expr) = case expr of
  E.Zero -> E.Zero
  E.One -> E.One
  E.Two -> E.Two
  E.Pi -> E.Pi
  E.Input -> E.Input
  E.Sqrt _ -> E.Sqrt ()
  E.Sin _ -> E.Sin ()
  E.Add _ _ -> E.Add () ()
  E.Sub _ _ -> E.Sub () ()
  E.Mul _ _ -> E.Mul () ()
  E.Div _ _ -> E.Div () ()
  E.Exp _ _ -> E.Exp () ()
  E.LT _ _ -> E.LT () ()
  E.ITE _ _ _ -> E.ITE () () ()

bigrams :: Expr -> [Bigram]
bigrams expr@(E.Fix node) = case node of
  E.Zero -> []
  E.One -> []
  E.Two -> []
  E.Pi -> []
  E.Input -> []
  E.Sqrt c -> bigram1 c
  E.Sin c -> bigram1 c
  E.Add c1 c2 -> bigram2 c1 c2
  E.Sub c1 c2 -> bigram2 c1 c2
  E.Mul c1 c2 -> bigram2 c1 c2
  E.Div c1 c2 -> bigram2 c1 c2
  E.Exp c1 c2 -> bigram2 c1 c2
  E.LT c1 c2 -> bigram2 c1 c2
  E.ITE c1 c2 c3 -> bigram3 c1 c2 c3
  where
    bigram1 :: Expr -> [Bigram]
    bigram1 c = [Bigram (tag expr) (tag c)]

    bigram2 :: Expr -> Expr -> [Bigram]
    bigram2 c1 c2 =
      [ Bigram (tag expr) (tag c1)
      , Bigram (tag expr) (tag c2)
      ]

    bigram3 :: Expr -> Expr -> Expr -> [Bigram]
    bigram3 c1 c2 c3 =
      [ Bigram (tag expr) (tag c1)
      , Bigram (tag expr) (tag c2)
      , Bigram (tag expr) (tag c3)
      ]

type Counts = M.Map Bigram Int

count :: [Bigram] -> Counts
count = foldr go M.empty
  where
  go :: Bigram -> Counts -> Counts
  go bigram = M.insertWith (+) bigram 1
