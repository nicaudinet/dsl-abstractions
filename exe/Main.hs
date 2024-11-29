module Main where

import Prelude hiding (LT)
import Data.List (intercalate)
import Control.Monad.Reader (Reader, ask, runReaderT)

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
  [ "if 0 < ", render e1
  , "then", render e2
  , "else", render e3
  ]

eval :: Expr -> Reader Float Float
eval Zero = 0
eval One = 1
eval Two = 2
eval Input = ask
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 / eval e2
eval (Exp e1 e2) = eval e1 ** eval e2
eval (LT e1 e2) = if eval e1 < eval e2 then 1 else -1
eval (Sqrt e) = sqrt (eval e)
eval (Sin e) = sin (eval e)
eval (ITE e1 e2 e3) = if 0 < eval e1 then eval e2 else eval e3

functions :: [Expr]
functions =
  [ Add Input One
  , Add (Exp Input Two) (Div Input (Sin Input))
  , Exp (Add Input Two) Two
  ]

main :: IO ()
main = mapM_ (putStrLn . render) functions
