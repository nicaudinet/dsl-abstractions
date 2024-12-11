{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module DSL
  ( Fix (..)
  , ExprF (..)
  , Expr
  , Tag
  , Bigram(..)
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
  , eval
  , size
  , subexpressions
  , render
  , bigrams
  ) where

import Data.List (intercalate)
import Test.QuickCheck (Arbitrary, arbitrary, frequency)
import Prelude hiding (LT, sqrt, div, exp, pi, sin)
import qualified Prelude as P (pi, sqrt, sin)

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

----------
-- Expr --
----------

data Fix f = Fix { unFix :: f (Fix f) }
deriving instance (Eq (f (Fix f))) => Eq (Fix f)
deriving instance (Ord (f (Fix f))) => Ord (Fix f)

type Expr = Fix ExprF

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

----------------
-- Evaluators --
----------------

-- | The result of "running" the expression
eval :: Expr -> Double -> Double
eval (Fix expr) i = case expr of
  Zero -> 0
  One -> 1
  Two -> 2
  Pi -> P.pi
  Input -> i
  Sqrt e -> P.sqrt (eval e i)
  Sin e -> P.sin (eval e i)
  Add e1 e2 -> eval e1 i + eval e2 i
  Sub e1 e2 -> eval e1 i - eval e2 i
  Mul e1 e2 -> eval e1 i * eval e2 i
  Div e1 e2 -> eval e1 i / eval e2 i
  Exp e1 e2 -> eval e1 i ** eval e2 i
  LT e1 e2 -> if eval e1 i < eval e2 i then 1 else -1
  ITE e1 e2 e3 -> if eval e1 i < 0 then eval e2 i else eval e3 i

-- | The number of nodes in an expression
size :: Expr -> Int
size (Fix expr) = case expr of
  Zero -> 1
  One -> 1
  Two -> 1
  Pi -> 1
  Input -> 1
  Sqrt e -> 1 + size e
  Sin e -> 1 + size e
  Add e1 e2 -> 1 + size e1 + size e2
  Sub e1 e2 -> 1 + size e1 + size e2
  Mul e1 e2 -> 1 + size e1 + size e2
  Div e1 e2 -> 1 + size e1 + size e2
  Exp e1 e2 -> 1 + size e1 + size e2
  LT e1 e2 -> 1 + size e1 + size e2
  ITE e1 e2 e3 -> 1 + size e1 + size e2 + size e3

-- | Find all possible subtrees of a given program
subexpressions :: Expr -> [Expr]
subexpressions (Fix expr) = Fix expr : case expr of
  Zero -> []
  One -> []
  Two -> []
  Pi -> []
  Input -> []
  Sqrt e -> subexpressions e
  Sin e -> subexpressions e
  Add e1 e2 -> subexpressions e1 <> subexpressions e2
  Sub e1 e2 -> subexpressions e1 <> subexpressions e2
  Mul e1 e2 -> subexpressions e1 <> subexpressions e2
  Div e1 e2 -> subexpressions e1 <> subexpressions e2
  Exp e1 e2 -> subexpressions e1 <> subexpressions e2
  LT e1 e2 -> subexpressions e1 <> subexpressions e2
  ITE e1 e2 e3 -> subexpressions e1 <> subexpressions e2 <> subexpressions e3

-- | Render the expression as a string
render :: Expr -> String
render (Fix expr) = case expr of
  Zero -> "0"
  One -> "1"
  Two -> "2"
  Pi -> "pi"
  Input -> "x"
  Sqrt e -> "sqrt" <> bracketed (render e)
  Sin e -> "sin" <> bracketed (render e)
  Add e1 e2 -> renderBinOp "+" e1 e2
  Sub e1 e2 -> renderBinOp "-" e1 e2
  Mul e1 e2 -> renderBinOp "*" e1 e2
  Div e1 e2 -> renderBinOp "/" e1 e2
  Exp e1 e2 -> renderBinOp "^" e1 e2
  LT e1 e2 -> renderBinOp "<" e1 e2
  ITE e1 e2 e3 -> bracketed $ intercalate " "
    [ "if", render e1, "< 0"
    , "then", render e2
    , "else", render e3
    ]
  where

  bracketed :: String -> String
  bracketed str = "(" <> str <> ")"

  renderBinOp :: String -> Expr -> Expr -> String
  renderBinOp op e1 e2 = bracketed $ render e1 <> " " <> op <> " " <> render e2

--------------------
-- Tag and Bigram --
--------------------

data Ariness = Nullary | Unary | Binary | Ternary

ariness :: ExprF a -> Ariness
ariness Zero = Nullary
ariness One = Nullary
ariness Two = Nullary
ariness Pi = Nullary
ariness Input = Nullary
ariness (Sqrt _) = Unary
ariness (Sin _) = Unary
ariness (Add _ _) = Binary
ariness (Sub _ _) = Binary
ariness (Mul _ _) = Binary
ariness (Div _ _) = Binary
ariness (Exp _ _) = Binary
ariness (LT _ _) = Binary
ariness (ITE _ _ _) = Ternary


type Tag = ExprF ()

instance Show Tag where
  show Zero = "0"
  show One = "1"
  show Two = "2"
  show Pi = "pi"
  show Input = "x"
  show (Sqrt _) = "sqrt"
  show (Sin _) = "sin"
  show (Add _ _) = "(+)"
  show (Sub _ _) = "(-)"
  show (Mul _ _) = "(*)"
  show (Div _ _) = "(/)"
  show (Exp _ _) = "(^)"
  show (LT _ _) = "(<)"
  show (ITE _ _ _) = "ITE"

data Position = First | Second | Third
  deriving (Eq, Ord)

data Bigram = Bigram Tag Tag Position
  deriving (Eq, Ord)

instance Show Bigram where
  show (Bigram parent child position) = showParent <> " -> " <> show child
    where
      showParent :: String
      showParent = case ariness parent of
        Nullary -> show parent
        Unary -> show parent <> " _"
        Binary -> case position of
          First -> "_ " <> show parent
          Second -> show parent <> " _"
          Third -> error "Binary operations don't have a third position"
        Ternary -> case position of
          First -> "if _ then else"
          Second -> "if then _ else"
          Third -> "if then _ else"
        
tag :: Expr -> Tag
tag (Fix expr) = case expr of
  Zero -> Zero
  One -> One
  Two -> Two
  Pi -> Pi
  Input -> Input
  Sqrt _ -> Sqrt ()
  Sin _ -> Sin ()
  Add _ _ -> Add () ()
  Sub _ _ -> Sub () ()
  Mul _ _ -> Mul () ()
  Div _ _ -> Div () ()
  Exp _ _ -> Exp () ()
  LT _ _ -> LT () ()
  ITE _ _ _ -> ITE () () ()

bigrams :: Expr -> [Bigram]
bigrams expr@(Fix node) = case node of
  Zero -> []
  One -> []
  Two -> []
  Pi -> []
  Input -> []
  Sqrt c -> bigram1 c
  Sin c -> bigram1 c
  Add c1 c2 -> bigram2 c1 c2
  Sub c1 c2 -> bigram2 c1 c2
  Mul c1 c2 -> bigram2 c1 c2
  Div c1 c2 -> bigram2 c1 c2
  Exp c1 c2 -> bigram2 c1 c2
  LT c1 c2 -> bigram2 c1 c2
  ITE c1 c2 c3 -> bigram3 c1 c2 c3

  where

    bigram1 :: Expr -> [Bigram]
    bigram1 c = [Bigram (tag expr) (tag c) First]

    bigram2 :: Expr -> Expr -> [Bigram]
    bigram2 c1 c2 =
      [ Bigram (tag expr) (tag c1) First
      , Bigram (tag expr) (tag c2) Second
      ]

    bigram3 :: Expr -> Expr -> Expr -> [Bigram]
    bigram3 c1 c2 c3 =
      [ Bigram (tag expr) (tag c1) First
      , Bigram (tag expr) (tag c2) Second
      , Bigram (tag expr) (tag c3) Third
      ]
