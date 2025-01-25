module ClosureConversion where

import Parser (
   Exp(Application, Prim, Lambda, DefineProc, If, Set, Varexp, Let),
   Var(Var),
   Binding(Binding),
   lexer,
   toAst)
import Desugar (desugar)
import ToAnf (toanf)
import qualified Data.Map as Map

data Env = Env { envMap :: Map.Map Var [Var] } deriving (Show, Eq)

data ClosureEnv = ClosureEnv { 
  fnEnv :: Map.Map Var [Var], 
  lambdaEnv :: Map.Map Var Var 
} deriving (Show, Eq)

closure :: [Exp] -> [Exp]
closure (x:xs) =
  closure' x (ClosureEnv { fnEnv = Map.empty, lambdaEnv = Map.empty}) 0 : rest
  where
    rest = closure xs
    
closure' :: Exp -> ClosureEnv -> Int -> Exp
closure' (DefineProc var params exp) env n =
  let lamName = Var ("lambda_" ++ show n) in
      if Map.member var (fnEnv env)
      then closure' exp env n
      else 
        let fnEnv' = Map.insert var params (fnEnv env)
            lambdaEnv' = Map.insert lamName lamName (lambdaEnv env)
        in (closure' exp (ClosureEnv { fnEnv = fnEnv', lambdaEnv = lambdaEnv' }) n)

closure' (Lambda vars exp) env n =
  let freeVars = freeVariables vars exp
      lamName = Var ("lambda_" ++ show n)
      tupName = Var ("tup_" ++ show n)
      fnEnv' = Map.insert tupName freeVars (fnEnv env)
      lambdaEnv' = Map.insert lamName lamName (lambdaEnv env)
      exp' = makeClosure'Exp exp freeVars tupName
  in DefineProc lamName vars (closure' exp' (ClosureEnv fnEnv' lambdaEnv') (n + 1))

closure' (If cnd thn els) env n =
  If (closure' cnd env n) (closure' thn env n) (closure' els env n)

closure' (Set var e) env n =
  Set var (closure' e env n)

closure' (Application (x:xs)) env n =
  case x of
    Varexp op ->
      if Map.member op (fnEnv env)
      then
        let tupName = Var ("tup_" ++ show n)
            tup = Application (Varexp (Var "tuple") : xs)
        in Let [Binding (Varexp tupName) tup] (Application (Varexp op : Varexp tupName : xs))
      else Application (x : xs)
    _ -> Application (x : xs)

closure' exp _ _ = exp 

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

freeVariables vars (If cnd thn els) =
  freeVariables vars cnd ++ freeVariables vars thn ++ freeVariables vars els

freeVariables vars (Lambda vars' exp) =
  filter (`notElem` vars') (freeVariables vars exp)

freeVariables _ _ = []

member :: (Eq a) => a -> [a] -> Bool
member x = any (== x)
