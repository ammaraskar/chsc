#include "../Prelude.hs"

data Expr = Var Var | Con Tag [Expr]
  | Lam Var Expr
  | App Expr Expr
  | Let [Binding] Expr
  | Case Expr [Alt] deriving (Eq, Show)

type Var = String
type Tag = String
type Binding = (Var, Expr)
type Alt = (Pat, Expr)
data Pat = Pat Tag [Var] deriving (Eq, Show)

type Chars = Int
data ParserResult a = Error String | Done a Chars String
newtype Parser a = Parser { parse :: String -> ParserResult a }
