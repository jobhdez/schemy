module ToAnf where

import Parser 
    ( Binding(Binding),
      Exp(Application, Let, Prim, If, Varexp, Bool, DefineProc, Lambda, Int, Cons, Nil, FunRef, Quote, Begin, Set, Cond),
      Var(Var),
      Operator(Plus),
      Cnd(Cnd, Else),
      lexer,
      toAst )

revealFunctions :: [Exp] -> [Exp]
revealFunctions [] = []
revealFunctions (x:xs) =
  reveal x : revealFunctions xs

reveal :: Exp -> Exp
reveal (Application (Varexp (Var var)) exps) =
  let exps' = map reveal exps in
    Application (FunRef (Var var) (length exps')) exps'

reveal (If cnd thn els) =
  (If (reveal cnd) (reveal thn) (reveal els))

reveal (Prim op e e2) =
  (Prim op (reveal e) (reveal e2))

reveal (DefineProc var vars exp) =
  (DefineProc var vars (reveal exp))

reveal (Lambda vars exp) =
  (Lambda vars (reveal exp))

{---
reveal (Let [Binding var exp] body) =
  (Let [Binding var (reveal exp)] (reveal body))
---}

reveal (Cons e e2) =
  Cons (reveal e) (reveal e2)

{--
reveal (Begin exps) =
  let rvs = map reveal exps in
    Begin rvs

reveal (Cond cnds) =
  let rvlconds = revealCnds cnds in
    Cond rvlconds
--}
reveal x = x
{---
revealCnds :: [Cnd] -> [Cnd]
revealCnds [] = []
revealCnds (x:xs) =
  case x of
    Cnd op exp ->
      [Cnd (reveal op) (reveal exp)] ++ revealCnds xs
    Else exp ->
      [Else (reveal exp)] ++ revealCnds xs
--}

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
  let tmp = Varexp (Var ("tmp" ++ show n))
      tmp2 = Varexp (Var ("tmp" ++ show (n+1))) in
    if (isatomic e) && (isatomic e2)
    then (Prim op e e2)
    else
      if (isatomic e) && not (isatomic e2) || (isatomic e2) && not (isatomic e)
      then
        if (isatomic e) then makeAnf e2 (Prim op e tmp2) tmp2 (n+1) else  (makeAnf e (Prim op tmp e2) tmp (n+1))
      else
        let e2' = (makeAnf e2 (Prim op tmp tmp2) tmp2  n) in
          (makeAnf e e2' tmp (n + 1))
    
toanf' (If (Bool op) thn els) n =
  (If (Bool op) (toanf' thn (n+1)) (toanf' els (n+1)))

toanf' (If cnd thn els) n =
  let tmp = ("tmp_" ++ show n) in
    (Let [Binding (Varexp (Var tmp)) (toanf' cnd (n + 1))] (If (Varexp (Var tmp)) (toanf' thn (n + 1)) (toanf' els (n + 1))))
   

toanf' (DefineProc var params exp) n =
  DefineProc var params (toanf' exp n)

toanf' (Lambda vars exp) n =
  (Lambda vars (toanf' exp n))

{---
toanf' (Let [Binding v (Varexp (Var v2))] body) n =
        (Let [Binding v (Varexp (Var v2))] (toanflet (toanf' body n)))

toanf' (Let [Binding v (Int n)] body) n' =
  (Let [Binding v (Int n)] (toanflet (toanf' body n')))

toanf' (Let [Binding v (If (Bool b) cnd els)] body) n =
  (Let [Binding v (If (Bool b) (toanflet (toanf' cnd n)) (toanflet (toanf' els n)))] (toanflet (toanf' body n)))

toanf' (Let [Binding v (If cnd thn els)] body) n =
  let let' = toanf' (If cnd thn els) n in
    let if' = getIf let' in
      let binding = getBind let' in
        (Let binding (Let [Binding v if'] (toanflet (toanf' body n))))

toanf' (Let [Binding v exp] body) n =
  (Let [Binding v exp] (toanflet (toanf' body n)))
--}

toanf' (Set v exp) n =
  (Set v (toanf' exp n))
  
toanf' (Cons e e2) n =
  let tmp = Varexp (Var ("tmp" ++ show n))
      tmp2 = Varexp (Var ("tmp" ++ show (n+1))) in
    if (isatomic e) && (isatomic e2)
    then Cons e e2
    else if (isatomic e) && not (isatomic e2) || (isatomic e2) && not (isatomic e) then
    if (isatomic e) then makeAnf e2 (Cons e tmp2) tmp2 (n+1) else makeAnf e (Cons tmp e2) tmp (n+1)
    else
    let e2' = makeAnf e2 (Cons tmp tmp2) tmp2  n in
          makeAnf e e2' tmp (n + 1)
          
toanf' (Application op exps) n =
  let exps' = map (\x -> (toanf' x n)) exps in
    appToAnf (Application op exps') n []

{--
toanf' (Begin exps) n =
  let bgns = map (\x -> (toanf' x n)) exps in
    Begin bgns


toanf' (Cond exps) n =
  let cnds = toanfcnd exps n in
    Cond cnds
---}
toanf' x n = x
toanfcnd :: [Cnd] -> Int -> [Cnd]
toanfcnd [] n = []
toanfcnd (x:xs) n =
  case x of
    Cnd op exp ->
      [Cnd (toanf' op n) (toanf' exp n)] ++ toanfcnd xs (n+1)
    Else exp ->
      [Else (toanf' exp n)]
appToAnf (Application (FunRef var n') []) n tmps =
  (Application (FunRef var n')) tmps
    
appToAnf (Application (FunRef var n') (x:xs)) n tmps =
  let tmp = Varexp (Var ("tmp" ++ show n))
      tmp2 = Varexp (Var ("tmp" ++ show n)) in
    case x of
      Int n ->
        let tmps' = x:tmps in
          appToAnf (Application (FunRef var n') xs) (n+1) tmps'
      Varexp (Var v) ->
        let tmps' = x:tmps in
          appToAnf (Application (FunRef var n') xs) (n+1) tmps'
      _->
        let anf' = toanf' x n
            tmps' = tmp:tmps
            app' = appToAnf (Application (FunRef var n') xs) (n+1) tmps' in
          makeAnf x app' tmp (n+1)
          
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
                              
isatomic :: Exp -> Bool
isatomic (Int n) = True
isatomic (Varexp (Var n)) = True
isatomic Nil = True
isatomic exp = False
  
getExps :: Exp -> [Exp]
getExps (Application op exps) =
  exps

toanflet :: Exp -> Exp
toanflet (Let [Binding var (Let [Binding var' exp'] body')] body) =
  getApp (Let [Binding var' exp'] body')  var body
toanflet exp = exp
getApp :: Exp -> Exp -> Exp -> Exp
getApp (Application fn exps) var'' body = (Let [Binding var'' (Application fn exps)] body)
getApp (Let [Binding var exp] (Let [Binding var' exp'] body')) var'' body =
  (Let [Binding var exp] (Let [Binding var' exp'] (getApp body' var'' body)))
  
getApp (Let [Binding var exp] (Prim op e e2)) var'' body =
        (Let [Binding var exp] (Let [Binding var'' (Prim op e e2)] body))
        
getDefExp :: Exp -> Exp
getDefExp (DefineProc var vars exp) = exp


