module Part2
  ( part2
  , enumerateUniform
  ) where

import Control.Applicative (asum)
import qualified Control.Monad.WeightedSearch as W
import DSL (Expr)
import qualified DSL as E

part2 :: IO ()
part2 = do
  putStrLn ""
  putStrLn "#######"
  putStrLn "Part 2:"
  putStrLn "#######"
  putStrLn ""

  putStrLn (pad 10 "Program:" <> "Likelihood:")
  mapM_ printWithProb (take 100 (W.toList enumerateUniform))
  putStrLn ""
  
  putStrLn (pad 25 "To Find:" <> pad 10 "Epsilon:" <> "Found:")
  findAndPrint "f(x) = x * 2" 0 (\x -> x * 2)
  findAndPrint "f(x) = abs(x)" 0.001 (\x -> abs x)
  findAndPrint "f(x) = x + 0.05" 0.1 (\x -> x + 0.05)
  findAndPrint "f(x) = x ** 2" 0.1 (\x -> x ** 2)
  findAndPrint "f(x) = (x ** 2) + 1" 0.1 (\x -> x ** 2 + 1)
  findAndPrint "f(x) = cos(x) ** 2" 0.001 (\x -> (cos x) ** 2)
  findAndPrint "f(x) = cos(x)" 0.001 (\x -> cos x)

  where

    pad :: Int -> String -> String
    pad n str = str <> replicate (n - length str) ' '

    printWithProb :: Expr -> IO ()
    printWithProb e = putStrLn (pad 20 (E.render e) <> show (log $ likelihood e))

    findAndPrint :: String -> Double -> (Double -> Double) -> IO ()
    findAndPrint title epsilon f = putStrLn $
      pad 25 title <> pad 10 (show epsilon) <> E.render (findProgram epsilon f)

likelihood :: Expr -> Double
likelihood expr = uniform ** (fromIntegral $ E.size expr)
  where
    uniform = 1 / 13 -- number of DSL nodes (without counting the input)

-- https://stackoverflow.com/questions/23515191/how-to-enumerate-a-recursive-datatype-in-haskell/23517557#23517557
-- https://web.archive.org/web/20140823135714/http://lukepalmer.wordpress.com/2008/05/02/enumerating-a-context-free-language/
enumerate :: [Int] -> W.T Int Expr
enumerate ws = asum $ zipWith W.weight ws
  [ pure E.zero
  , pure E.one
  , pure E.two
  , pure E.pi
  , pure E.input
  , E.sqrt <$> enumerate ws
  , E.sin <$> enumerate ws
  , E.add <$> enumerate ws <*> enumerate ws
  , E.sub <$> enumerate ws <*> enumerate ws
  , E.mul <$> enumerate ws <*> enumerate ws
  , E.div <$> enumerate ws <*> enumerate ws
  , E.exp <$> enumerate ws <*> enumerate ws
  , E.ite <$> enumerate ws <*> enumerate ws <*> enumerate ws
  ]

enumerateUniform :: W.T Int Expr
enumerateUniform = enumerate (repeat 1)

linspace :: Double -> Double -> Int -> [Double]
linspace start stop num = map scale [0 .. num]
  where
    scale :: Int -> Double
    scale i = start + (fromIntegral i) * (stop - start) / (fromIntegral num)

type Dataset = [(Double, Double)]

makeDataset :: (Double -> Double) -> Dataset
makeDataset fn =
  let ins = linspace (-2) 2 50
  in zip ins (map fn ins)

testProgram :: Double -> Expr -> Double -> Double -> Bool
testProgram epsilon expr input output =
  abs (output - (E.eval expr input)) <= epsilon

testAll :: Double -> (Double -> Double) -> Expr -> Maybe Expr
testAll epsilon fn expr =
  if all (uncurry (testProgram epsilon expr)) (makeDataset fn)
  then Just expr
  else Nothing

unfoldUntil :: (a -> Either a b) -> a -> b
unfoldUntil f a = either (unfoldUntil f) id (f a)

findProgram :: Double -> (Double -> Double) -> Expr 
findProgram epsilon fn = unfoldUntil go (W.toList enumerateUniform) where
  go :: [Expr] -> Either [Expr] Expr
  go [] = error "infinite lists should never end..."
  go (p:ps) = maybe (Left ps) Right (testAll epsilon fn p)
