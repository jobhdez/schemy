module ToAnf where

import Parser 
    ( Binding(Binding),
      Exp(Application, Let, Prim, If, Varexp, Bool, DefineProc, Lambda, Int),
      Var(Var),
      lexer,
      toAst )

toAnf :: [Exp] -> [Exp]
toAnf [] = []
toAnf (x:xs) =
  toanf x : rest
  where
    rest = toAnf xs
    
toanf :: Exp -> Exp
toanf exp =
  toanf' exp 0


toanf' :: Exp -> Int ->  Exp

toanf' (If (Bool op) thn els) n =
  let tmp = "tmp_" ++ show n in
    (Let [Binding (Varexp (Var tmp)) (Bool op)] (If (Varexp (Var tmp)) (toanf' thn (n+1)) (toanf' els (n+1))))
    
toanf' (If (Prim op e e2) thn els) n =
  let tmp = "tmp_" ++ show n in
    (Let [Binding (Varexp (Var tmp)) (Prim op e e2)] (If (Varexp (Var tmp)) (toanf' thn (n+1)) (toanf' els (n+1))))

toanf' (If cnd thn els) n =
  let tmp = ("tmp_" ++ show n) in
    (Let [Binding (Varexp (Var tmp)) (toanf' cnd (n + 1))] (If (Varexp (Var tmp)) (toanf' thn (n + 1)) (toanf' els (n + 1))))
   

toanf' (DefineProc var params exp) n =
  DefineProc var params (toanf' exp n)
  
toanf' (Application op exps) n =
  let tmp = "tmp" ++ show n in
    (Let [Binding (Varexp (Var tmp)) op] (Application op (toAnf exps)))

toanf' (Lambda vars exp) n =
  (Lambda vars (toanf' exp n))

toanf' (Let [Binding v (Varexp (Var v2))] body) n =
        (Let [Binding v (Varexp (Var v2))] (toanf' body n))

toanf' (Let [Binding v (Int n)] body) n' =
  (Let [Binding v (Int n)] (toanf' body n'))

toanf' (Let [Binding v exp] body) n =
  let tmp = (Varexp (Var ("tmp"++ show n))) in
    (Let [Binding tmp (toanf' exp (n+1))] (toanf' body (n+1)))
 
        

toanf' x n = x
