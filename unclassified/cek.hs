-- http://matt.might.net/articles/cek-machines/
type VarName = String
type Lambda = (VarName, Exp)
type Binder = (VarName, Exp)
data Exp = Var VarName
  | Fn Lambda
  | Ap Exp Exp
  | Num Int
  | Add Exp Exp
  | Mul Exp Exp
  | Paren Exp
  deriving (Eq, Show)

type State = (Exp, Env, Kont)
data Frame = Closure (Lambda, Env)
  | Val Int
  
type Env = VarName -> Frame
data Kont = Empty
  | Arg (Exp, Env, Kont)
  | Func (Lambda, Env, Kont)
  | Addition (Int, Env, Kont) 
  | AddRight (Exp, Env, Kont)

compile :: Exp -> State
compile exp = (exp, initialEnv, Empty)

initialEnv :: Env
initialEnv x = error $ "no binding for " ++ x

step :: State -> State

step (Var name, env, kont) = case env name of
  Closure (lambda, env') -> (Fn lambda, env', kont)
  Val n -> (Num n, env, kont)

step (Add exp0 exp1, env, kont) = (exp0, env, AddRight (exp1, env, kont))

step (Num n, env, AddRight (exp, env', kont)) = (exp, env', Addition (n, env, kont))

step (Num n, env, Addition (x, env', kont)) = (Num (n + x), env', kont)

step (Ap exp0 exp1, env, kont) = (exp0, env, Arg (exp1, env, kont))

step (Fn lambda, env, Arg (exp, env', kont)) = (exp, env', Func (lambda, env, kont))

step (arg, env, Func ((var, exp), env', kont)) = case arg of

  Fn lambda -> (exp, env'', kont)
    where env'' var' | var' == var = Closure (lambda, env)
                     | otherwise = env' var'

  Num n -> (exp, env'', kont)
    where env'' var' | var' == var = Val n
                     | otherwise = env' var'

evaluate :: State -> State
evaluate state@(Num _, env, Empty) = state
evaluate state = evaluate $ step state

showResult :: State -> Int
showResult (Num n, env, kont) = n
showResult (exp, _, _) = error $ "shwo result error: " ++ (show exp)

interpreter :: Exp -> Int
interpreter = showResult.evaluate.compile

id' = Fn ("x", Var "x")
const' = Fn ("x", Fn ("y", Var "x"))

sum_1_2_3_4 = Add (Add (Num 1) (Num 2)) (Add (Num 3) (Num 4))

test = interpreter $ Ap id' sum_1_2_3_4