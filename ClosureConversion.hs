module ClosureConversion where

import Parser (
   Exp(Application, Prim, Lambda, DefineProc, If, Set, Varexp, Let, Int, Closure),
   Var(Var),
   Binding(Binding),
   lexer,
   toAst)
import Desugar (desugar)
import ToAnf (toanf, toAnf)
import qualified Data.Map as Map

data ClosureEnv = ClosureEnv { 
  fvEnv :: Map.Map Var [Var],
  lambdaEnv :: Map.Map Var Exp
} deriving (Eq)

instance Show ClosureEnv where
  show (ClosureEnv fvEnv lambdaEnv) =
    "fnEnv: " ++ show fvEnv ++ ", lambdaEnv: " ++ show lambdaEnv

closureConversion :: [Exp] -> ([Exp], ClosureEnv)
closureConversion exps =
  closure exps (ClosureEnv { fvEnv = Map.empty, lambdaEnv = Map.empty })
  
closure :: [Exp] -> ClosureEnv -> ([Exp], ClosureEnv)
closure [] env = ([], env)
closure (x:xs) env =
  let (x', env'') = closure' x env' 0        
      (xs', env''') = closure xs env''       
  in (x' ++ xs', env''')                     
  where
    env' = env 

closure' :: Exp -> ClosureEnv -> Int -> ([Exp], ClosureEnv)
closure' (DefineProc var params exp) env n =
  case exp of
    Application (op:xs) ->
      case op of
        Lambda var' (Lambda var'' exp') ->
          let (clsr, env') = closure' op env n
              ---(exps, env'') = closure xs env'
              fvs = freeVariables var'' exp'
              --lamvars = getLamVars (head clsr)
              freeName = "fvs_" ++ show n
              freevars = Map.insert (Var freeName) fvs (fvEnv env')
             -- lambName = "lambda_" ++ show n
              {--closuree = Closure
                          (Int (length (getLamVars (head clsr))))
                          (Varexp (Var ("lamb_" ++ show n)))
                          (Int (length (getLamVars (head clsr))))
                          fvs--}
              lambName = getClosureName (head clsr)
              lamenv = lambdaEnv env'
              --lamEnv = Map.insert (Var lambName) lambdaexp (lambdaEnv env')
              def = clsr
              (exps, env''') = closure xs (ClosureEnv { fvEnv = Map.empty, lambdaEnv = Map.empty })
              ops = [Varexp (Var "tupleref"), (Varexp lambName), Int 0] ++ exps
              app = (Application ops)
          in (def ++ [app], ClosureEnv {fvEnv = freevars, lambdaEnv = lamenv})
        _ ->
          closure' (Application (op:xs)) env n

closure' (Lambda vars exp) env n =
  case exp of
    (Lambda vars' exp') ->
      let (e, v) = closure' (Lambda vars' exp') env n in
        let defExp = getDefExp (head e) in
          let vars'' = getDefVars (head e) in
            let fvs = freeVariables vars' exp' in
              let fvsEnv' = Map.insert (Var ("fvs_" ++ show n)) fvs (fvEnv v) in
                let lamEnv' = Map.insert (Var ("lamb_" ++ show n)) (Lambda vars' exp') (lambdaEnv v) in
                  ([DefineProc (Var ("lamb_" ++ show n)) ([Var ("fvs_" ++ show n)] ++ vars) (Closure (Int (length vars')) (Varexp (Var ("lamb_" ++ show (n+1)))) (Int (length vars'')) fvs), (DefineProc (Var ("lamb_" ++ (show (n+1)))) ([Var ("fvs_" ++ show n)] ++ vars') (makeClosure'Exp exp' fvs (Var ("fvs_" ++ show n))))], (ClosureEnv { fvEnv = fvsEnv', lambdaEnv = lamEnv' }))
    _ ->
      let freeVars = freeVariables vars exp
          lamName = ("lambda_" ++ show n)
          fvName = Var ("fv_" ++ show n)
          fvEnv' = Map.insert fvName freeVars (fvEnv env)
          lambdaEnv' = Map.insert (Var lamName) (Lambda vars exp) (lambdaEnv env)
          exp' = makeClosure'Exp exp freeVars fvName
          clsr = Closure (Int (length vars)) (Varexp (Var ("lamb_" ++ show n))) (Int (length vars)) freeVars
      in
        ([DefineProc (Var lamName) ([fvName] ++ vars) (makeClosure'Exp exp freeVars fvName)], ClosureEnv { fvEnv = fvEnv', lambdaEnv = lambdaEnv' })
       

closure' (If cnd thn els) env n =
  let (cnd', env') = closure' cnd env n
      (thn', env'') = closure' thn env' n
      (els', env''') = closure' els env'' n
  in ([If (head cnd') (head thn') (head els')], env''') 

closure' (Set var e) env n =
  let (e', env') = closure' e env n
  in ([Set var (head e')], env') 

closure' (Application (x:xs)) env n =
  case x of
    Varexp op ->
      if Map.member op (fvEnv env)
      then
        let tupName = Var ("tup_" ++ show n)
            tup = Application (Varexp (Var "tuple") : xs)
        in 
          let (tup', env') = closure' tup env n
              (app', env'') = closure' (Application (Varexp op : Varexp tupName : xs)) env' n
          in ([Let [Binding (Varexp tupName) (head tup')] (head app')], env'')
      else 
        let (xs', env') = closure' (Application xs) env n
        in ([Application (x : xs')], env')
      
closure' exp env _ = ([exp], env)  

makeClosure'Exp :: Exp -> [Var] -> Var -> Exp
makeClosure'Exp exp [] _ = exp
makeClosure'Exp exp (x:xs) env =
  Let [Binding (Varexp x) (Application [Varexp (Var "tupleref"), Varexp env, Varexp x])] (makeClosure'Exp exp xs env)

freeVariables :: [Var] -> Exp -> [Var]
freeVariables vars (Set (Varexp (Var varName)) exp) =
  if Var varName `elem` vars then [] else [Var varName]

freeVariables vars (Prim _ (Varexp (Var e1)) (Varexp (Var e2))) =
  let fv1 = if Var e1 `elem` vars then [] else [Var e1]
      fv2 = if Var e2 `elem` vars then [] else [Var e2]
  in fv1 ++ fv2

freeVariables vars (Prim _ (Varexp (Var e1)) e2) =
  let fv1 = if Var e1 `elem` vars then [] else freeVariables vars e2
      fv2 = freeVariables vars e2
  in fv1 ++ fv2
  
freeVariables vars (If cnd thn els) =
  freeVariables vars cnd ++ freeVariables vars thn ++ freeVariables vars els

freeVariables vars (Lambda vars' exp) =
  filter (`notElem` vars') (freeVariables vars exp)

freeVariables _ _ = []

member :: (Eq a) => a -> [a] -> Bool
member x = any (== x)

getDefExp :: Exp -> Exp
getDefExp (DefineProc var vars e) = e

getDefVars :: Exp -> [Var]
getDefVars (DefineProc var vars e) = vars

getLamVars :: Exp -> [Var]
getLamVars (Lambda vars exp) = vars

getLamExp :: Exp -> Exp
getLamExp (Lambda vars exp) = exp

getClosureName :: Exp -> Var
getClosureName (DefineProc var vars (Closure n (Varexp (Var v)) n2 fvs)) =
  Var v
