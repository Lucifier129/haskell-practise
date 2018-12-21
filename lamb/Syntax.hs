type VarName = String
type ArgName = String
data Binding = Binding VarName Expr
type BindingList = [Binding]

data Expr = FNum Float
  | Var VarName
  | App Expr Expr
  | Fn [ArgName] Expr
  | Lambda [ArgName] Expr
  | BindingExpr Binding
  | LetExpr BindingList Expr
  | CaseExpr Expr [(Expr, Expr)]
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Neg Expr
  deriving (Show, Eq)

type Prog = [Expr]