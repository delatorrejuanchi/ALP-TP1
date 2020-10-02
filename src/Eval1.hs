module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
-- Completar la definición
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Pair Comm State
stepComm (Skip) s = Skip :!: s
stepComm (Let v e) s = Skip :!: update v nv s'
                       where (nv :!: s') = evalExp e s
stepComm (Seq Skip c1) s = c1 :!: s
stepComm (Seq c0 c1) s = Seq c0' c1 :!: s'
                         where (c0' :!: s') = stepComm c0 s
stepComm (IfThenElse b c0 c1) s = (if bv then c0 else c1) :!: s'
                                  where (bv :!: s') = evalExp b s
stepComm w@(While b c) s = (if bv then Seq c w else Skip) :!: s'
                           where (bv :!: s') = evalExp b s

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Pair a State
evalExp (Const nv) s = nv :!: s
evalExp (Var v) s = lookfor v s :!: s
evalExp (UMinus e) s = -n :!: s'
                       where (n :!: s') = evalExp e s
evalExp (Plus e0 e1) s = nv0 + nv1 :!: s''
                         where (nv0 :!: s') = evalExp e0 s
                               (nv1 :!: s'') = evalExp e1 s'
evalExp (Minus e0 e1) s = nv0 - nv1 :!: s''
                          where (nv0 :!: s') = evalExp e0 s
                                (nv1 :!: s'') = evalExp e1 s'
evalExp (Times e0 e1) s = n0 * n1 :!: s''
                          where (n0 :!: s') = evalExp e0 s
                                (n1 :!: s'') = evalExp e1 s'
evalExp (Div e0 e1) s = div n0 n1 :!: s''
                        where (n0 :!: s') = evalExp e0 s
                              (n1 :!: s'') = evalExp e1 s'
evalExp (EAssgn v e) s = nv :!: update v nv s'
                         where (nv :!: s') = evalExp e s
evalExp (ESeq e0 e1) s = evalExp e1 $ Data.Strict.Tuple.snd $ evalExp e0 s
-- Binary
evalExp BTrue s = True :!: s
evalExp BFalse s = False :!: s
evalExp (Lt e0 e1) s = (n0 < n1) :!: s''
                       where (n0 :!: s') = evalExp e0 s
                             (n1 :!: s'') = evalExp e1 s'
evalExp (Gt e0 e1) s = (n0 > n1) :!: s''
                       where (n0 :!: s') = evalExp e0 s
                             (n1 :!: s'') = evalExp e1 s'
evalExp (Eq e0 e1) s = (n0 == n1) :!: s''
                       where (n0 :!: s') = evalExp e0 s
                             (n1 :!: s'') = evalExp e1 s'
evalExp (NEq e0 e1) s = (n0 /= n1) :!: s''
                        where (n0 :!: s') = evalExp e0 s
                              (n1 :!: s'') = evalExp e1 s'
evalExp (And p0 p1) s = (b0 && b1) :!: s''
                        where (b0 :!: s') = evalExp p0 s
                              (b1 :!: s'') = evalExp p1 s'
evalExp (Or p0 p1) s = (b0 || b1) :!: s''
                        where (b0 :!: s') = evalExp p0 s
                              (b1 :!: s'') = evalExp p1 s'
evalExp (Not p) s = not bv :!: s'
                     where (bv :!: s') = evalExp p s
