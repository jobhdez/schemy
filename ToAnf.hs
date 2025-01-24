module ToAnf where

import Parser
    ( Binding(Binding),
      Exp(Application, Let, Prim, If, Varexp, Bool),
      Var(Var),
      lexer,
      toAst )
import Desugar ( desugar )

toanf :: Exp -> Exp
toanf exp =
  toanf' exp 0


toanf' :: Exp -> Int ->  Exp

toanf' (If (Prim op e e2) thn els) n =
  let tmp = "tmp_" ++ show n in
    desugar(Let [Binding (Varexp (Var tmp)) (Prim op e e2)] (If (Varexp (Var tmp)) (toanf thn) (toanf els)))

toanf' (If cnd thn els) n =
  case cnd of
    (If cnd' thn' els') ->
      let tmp = "tmp_" ++ show n in
        let tmp2 = "tmp_" ++ show (n + 1) in
          case cnd' of
            Bool cnd'' ->
              desugar (Let [Binding (Varexp (Var tmp)) (If cnd' (toanf' thn' n) (toanf' els' n))] (If (Varexp (Var tmp)) (toanf' thn n) (toanf' els n)))
            Prim op e e2 ->
              desugar (Let [Binding (Varexp (Var tmp)) (Prim op e e2)] (Let [Binding (Varexp (Var tmp2)) (If (Varexp (Var tmp)) (toanf' thn' n) (toanf' els' n))] (If (Varexp (Var tmp2)) (toanf' thn n) (toanf' els n))))
            _  -> toanf' cnd' n
         

toanf' (Application exps) n =
  let op = head exps in
    let (x:xs) = tail exps in
      toanf'' op n (x:xs)

toanf' exp n =
  exp

toanf'' :: Exp -> Int -> [Exp] -> Exp
toanf'' op n (x:xs) =
  case x of
    If cnd' thn' els' ->
      let tmp = "tmp_" ++ show n
          tmp2 = "tmp_" ++ show (n + 1)
      in case cnd' of
           Bool cnd'' ->
             let exps = (map (\x -> toanf' x n) xs) in
               desugar
               (Let [Binding (Varexp (Var tmp)) (If cnd' (toanf' thn' n) (toanf' els' n))] (Application (op : exps)))
                 
           Prim op' e e2 ->
             let exps = (map (\x -> toanf' x n) xs) in
               desugar
               (Let [ Binding (Varexp (Var tmp)) (Prim op' e e2)] (Let [Binding (Varexp (Var tmp2)) (If (Varexp (Var tmp)) (toanf' thn' n) (toanf' els' n))] (Application (op : exps))))
           _ -> toanf'' op (n + 1) xs
    _ ->
      let exps = (map (\x -> toanf' x n) xs) in
        Application ([op] ++ exps)
