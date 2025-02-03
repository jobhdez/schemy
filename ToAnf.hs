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

toanf' (Prim op e e2) n =
  let tmpvar = Varexp (Var ("tmp" ++ show n))
      tmpvar2 = Varexp (Var ("tmp" ++ show (n+1)))
      e2' = makeAnf e2 (Prim op tmpvar tmpvar2) tmpvar2 n in
    makeAnf e e2' tmpvar n
    
toanf' (If (Bool op) thn els) n =
  (If (Bool op) (toanf' thn (n+1)) (toanf' els (n+1)))
    
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
    (Let [Binding (Varexp (Var tmp)) op] (Application (Varexp (Var tmp)) (toAnf exps)))

toanf' (Lambda vars exp) n =
  (Lambda vars (toanf' exp n))

toanf' (Let [Binding v (Varexp (Var v2))] body) n =
        (Let [Binding v (Varexp (Var v2))] (toanf' body n))

toanf' (Let [Binding v (Int n)] body) n' =
  (Let [Binding v (Int n)] (toanf' body n'))

toanf' (Let [Binding v (If (Bool b) cnd els)] body) n =
  (Let [Binding v (If (Bool b) (toanf' cnd n) (toanf' els n))] (toanf' body n))

toanf' (Let [Binding v (If cnd thn els)] body) n =
  let let' = toanf' (If cnd thn els) n in
    let if' = getIf let' in
      let binding = getBind let' in
        (Let binding (Let [Binding v if'] (toanf' body n)))

toanf' (Let [Binding v exp] body) n =
  (Let [Binding v exp] (toanf' body n))
  
toanf' x n = x

getIf :: Exp -> Exp
getIf (Let [Binding v e] (If cnd thn els)) =
  (If cnd thn els)

getBind :: Exp -> [Binding]
getBind (Let binding (If cnd thn els)) =
  binding

makeAnf :: Exp -> Exp -> Exp -> Int -> Exp
makeAnf e x y n =
  let anf' = toanf' e n in
    case anf' of
      Let binding body ->
        Let binding (Let [Binding y body] x)
        
      _ ->
        Let [Binding y e] x
                               
