module Part3
  ( part3
  ) where

import qualified Control.Monad.WeightedSearch as W
import Data.Bifunctor (second)
import Data.List (sortOn)
import qualified Data.Map as M 
import Data.Maybe (catMaybes)
import Graphics.Gnuplot.Simple
  ( Attribute(..)
  , LineSpec(..)
  , LineAttr(..)
  , PlotStyle
  , plotPathsStyle
  , lineSpec
  , defaultStyle
  )

import qualified DSL as E
import Part2 (enumerateUniform)

part3 :: IO ()
part3 = do
  putStrLn ""
  putStrLn "#######"
  putStrLn "Part 3:"
  putStrLn "#######"
  putStrLn ""

  -- Load the data in
  (xs, values) <- readDatasets

  -- plotValues xs (take 4 values)
  let (bestPrograms, meanError) = bestFits xs values (W.toList enumerateUniform)
  putStrLn ("Mean Error: " <> show meanError)
  putStrLn ""

  let bigramCounts = countBigrams bestPrograms
  putStrLn (showBigramCounts bigramCounts)

  let subexpressionCounts = countSubexpressions bestPrograms
  putStrLn (showSubexpressionCounts subexpressionCounts)


splitOn :: Eq a => a -> [a] -> [[a]]
splitOn delimiter = map reverse . go []
  where
    go accum [] = [accum]
    go accum (x:xs)
      | x == delimiter = accum : go [] xs
      | otherwise = go (x : accum) xs

readDatasets :: IO ([Double], [[Double]])
readDatasets = do
  contents <- readFile "../values.csv"
  let seqs = map (map read . splitOn ',') (lines contents)
  let xs = head seqs
  pure (xs, tail seqs)

-- Finding the best fits

bestFits :: [Double] -> [[Double]] -> [E.Expr] -> ([E.Expr], Double)
bestFits xs values programs =
  let progs = filterPrograms (take 50000 programs)
  in (sortPrograms progs, meanError progs)
  where

    evaluateProgram :: E.Expr -> Maybe (E.Expr, [Double])
    evaluateProgram prog =
      if any invalid outs then Nothing else Just (prog, outs)
      where
        outs :: [Double]
        outs = map (E.eval prog) xs

        invalid :: Double -> Bool
        invalid x = isNaN x || x < -2 || 2 <= x

    filterPrograms :: [E.Expr] -> [(E.Expr, [Double])]
    filterPrograms = catMaybes . map evaluateProgram

    errors :: [Double] -> [Double]
    errors outs = map (sum . zipWith (\x y -> (x - y) **2) outs) values

    sortPrograms :: [(E.Expr, [Double])] -> [E.Expr]
    sortPrograms = map fst . sortOn snd . map (second (sum . errors))

    mean :: [Double] -> Double
    mean as = sum as / (fromIntegral $ length as)

    meanError :: [(E.Expr, [Double])] -> Double
    meanError = mean . map (minimum . errors . snd)

-- Plot the graphs


plotValues :: [Double] -> [[Double]] -> IO ()
plotValues xs = plotPathsStyle attributes . zipWith toPlotItem styles
  where

    attributes :: [Attribute]
    attributes =
      [ Title "Values"
      , Custom "linetype" ["1", "linecolor", "\"blue\""]
      , Custom "linetype" ["2", "linecolor", "\"dark-yellow\""]
      , Custom "linetype" ["3", "linecolor", "\"forest-green\""]
      , Custom "linetype" ["4", "linecolor", "\"red\""]
      ]

    styles :: [[LineAttr]]
    styles =
      [ [LineTitle "values[0]", LineType 1, LineWidth 2, PointType 7, PointSize 15]
      , [LineTitle "values[1]", LineType 2, LineWidth 2, PointType 7, PointSize 15]
      , [LineTitle "values[2]", LineType 3, LineWidth 2, PointType 7, PointSize 15]
      , [LineTitle "values[3]", LineType 4, LineWidth 2, PointType 7, PointSize 15]
      ]

    toPlotItem :: [LineAttr] -> [Double] -> (PlotStyle, [(Double, Double)])
    toPlotItem style values =
      (defaultStyle { lineSpec = CustomStyle style }, (zip xs values))

-- Fit a bigram probability distribution to the best programs

type BigramCounts = M.Map E.Bigram Int

countBigrams :: [E.Expr] -> BigramCounts
countBigrams = foldr go M.empty . concatMap E.bigrams
  where
    go :: E.Bigram -> BigramCounts -> BigramCounts
    go bigram = M.insertWith (+) bigram 1

showBigramCounts :: BigramCounts -> String
showBigramCounts = unlines . showPaddedCount . reverse . sortOn snd . M.toList
  where

    pad :: Int -> String -> String
    pad n str = str <> concat (replicate (n - (length str)) " ")

    showPaddedCount :: [(E.Bigram, Int)] -> [String]
    showPaddedCount sortedCounts =
      let xs = map (show . fst) sortedCounts
          ys = map (show . snd) sortedCounts
          n = 10 + maximum (map length xs)
          title = pad n "Bigram:" <> "Count:"
      in title : zipWith (<>) (map (pad n) xs) ys

type ExprCounts = M.Map E.Expr Int

countSubexpressions :: [E.Expr] -> ExprCounts
countSubexpressions = foldr go M.empty . concatMap E.subexpressions
  where
    go :: E.Expr -> ExprCounts -> ExprCounts
    go expr = M.insertWith (+) expr 1

showSubexpressionCounts :: ExprCounts -> String
showSubexpressionCounts = unlines . showPaddedCount . reverse . sortOn snd . take 100 . M.toList
  where

    pad :: Int -> String -> String
    pad n str = str <> concat (replicate (n - (length str)) " ")

    showPaddedCount :: [(E.Expr, Int)] -> [String]
    showPaddedCount sortedCounts =
      let xs = map (E.render . fst) sortedCounts
          ys = map (show . snd) sortedCounts
          n = 10 + maximum (map length xs)
          title = pad n "Expr:" <> "Count:"
      in title : zipWith (<>) (map (pad n) xs) ys
