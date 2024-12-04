module DSL
  ( Fix (..)
  , ExprF (..)
  , Expr
  , zero
  , one
  , two
  , pi
  , input
  , sqrt
  , sin
  , add
  , sub
  , mul
  , div
  , exp
  , lt
  , ite
  ) where

import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Prelude hiding (LT, sqrt, div, exp, pi, sin)

data ExprF x
  = Zero
  | One
  | Two
  | Pi
  | Input
  | Sqrt x
  | Sin x
  | Add x x
  | Sub x x
  | Mul x x
  | Div x x
  | Exp x x
  | LT x x
  | ITE x x x
  deriving (Eq, Ord)

instance Arbitrary x => Arbitrary (ExprF x) where
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

data Fix f = Fix { unFix :: f (Fix f) }
type Expr = Fix ExprF

------------------
-- Constructors --
------------------

zero :: Expr
zero = Fix Zero

one :: Expr
one = Fix One

two :: Expr
two = Fix Two

pi :: Expr
pi = Fix Pi

input :: Expr
input = Fix Input

sqrt :: Expr -> Expr
sqrt = Fix . Sqrt

sin :: Expr -> Expr
sin = Fix . Sin

add :: Expr -> Expr -> Expr
add a b = Fix (Add a b)

sub :: Expr -> Expr -> Expr
sub a b = Fix (Sub a b)

mul :: Expr -> Expr -> Expr
mul a b = Fix (Mul a b)

div :: Expr -> Expr -> Expr
div a b = Fix (Div a b)

exp :: Expr -> Expr -> Expr
exp a b = Fix (Exp a b)

lt :: Expr -> Expr -> Expr
lt a b = Fix (LT a b)

ite :: Expr -> Expr -> Expr -> Expr
ite a b c = Fix (ITE a b c)
