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
    desugar (Let [Binding (Varexp (Var tmp)) (Prim op e e2)] (If (Varexp (Var tmp)) (toanf thn) (toanf els)))

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
  Application (map (\x -> (toanf' x n)) exps)


toanf' exp n =
  exp
