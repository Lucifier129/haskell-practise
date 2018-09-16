-- http://matt.might.net/articles/cek-machines/
type VarName = String
type Lambda = (VarName, Exp)
data Exp = Var VarName | Fn Lambda | Ap Exp Exp deriving (Eq, Show)

type State = (Exp, Env, Kont)
data Frame = Closure (Lambda, Env)
type Env = VarName -> Frame
data Kont = Empty | Arg (Exp, Env, Kont) | Func (Lambda, Env, Kont)

compile :: Exp -> State
compile exp = (exp, env, Empty) where env x = error $ "no binding for " ++ x

step :: State -> State

step (Var name, env, kont) = (Fn lambda, env', kont)
  where Closure (lambda, env') = env name

step (Ap fn exp, env, kont) = (fn, env, Arg (exp, env, kont))

step (Fn lambda, env, Arg (exp, env', kont)) = (exp, env', Func (lambda, env, kont))

step (Fn lambda, env, Func ((var, exp), env', kont)) = (exp, env'', kont)
  where
    env'' var' | var' == var = Closure (lambda, env)
               | otherwise = env' var'

evaluate :: State -> State
evaluate state@(Fn lambda, env, Empty) = state
evaluate state = evaluate $ step state

showResult :: State -> Exp
showResult (exp, env, kont) = exp

id' = Fn ("x", Var "x")
const' = Fn ("x", Fn ("y", Var "x"))

test = evaluate (compile $ Ap id' id')

test' = evaluate (compile $ Ap (Ap const' const') id')

-- interpreter :: Prog -> State
-- interpreter prog = showResult.evaluate.compile.parser