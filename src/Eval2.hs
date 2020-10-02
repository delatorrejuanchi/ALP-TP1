module Eval2
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
lookfor :: Variable -> State -> Either Error Int
lookfor v s = case s M.!? v of Nothing -> Left UndefVar
                               Just nv -> Right nv

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> Either Error State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> Either Error State
stepCommStar Skip s = return s
stepCommStar c    s = do (c' :!: s') <- stepComm c s
                         stepCommStar c' s'

-- Evalua un paso de un comando en un estado dado
-- Completar la definición
stepComm :: Comm -> State -> Either Error (Pair Comm State)
stepComm (Skip) s = Right (Skip :!: s)
stepComm (Let v e) s = case evalExp e s of Right (nv :!: s') -> Right (Skip :!: update v nv s')
                                           Left error -> Left error
stepComm (Seq Skip c1) s = Right (c1 :!: s)
stepComm (Seq c0 c1) s = case stepComm c0 s of Right (c0' :!: s') -> Right (Seq c0' c1 :!: s')
                                               Left error -> Left error
stepComm (IfThenElse b c0 c1) s = case evalExp b s of Right (bv :!: s') -> Right ((if bv then c0 else c1) :!: s')
                                                      Left error -> Left error
stepComm w@(While b c) s = case evalExp b s of Right (bv :!: s') -> Right ((if bv then Seq c w else Skip) :!: s')
                                               Left error -> Left error

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp = undefined
-- evalExp (Const nv) s = nv :!: s
-- evalExp (Var v) s = lookfor v s :!: s
-- evalExp (UMinus e) s = -n :!: s'
--                        where (n :!: s') = evalExp e s
-- evalExp (Plus e0 e1) s = nv0 + nv1 :!: s''
--                          where (nv0 :!: s') = evalExp e0 s
--                                (nv1 :!: s'') = evalExp e1 s'
-- evalExp (Minus e0 e1) s = nv0 - nv1 :!: s''
--                           where (nv0 :!: s') = evalExp e0 s
--                                 (nv1 :!: s'') = evalExp e1 s'
-- evalExp (Times e0 e1) s = n0 * n1 :!: s''
--                           where (n0 :!: s') = evalExp e0 s
--                                 (n1 :!: s'') = evalExp e1 s'
-- evalExp (Div e0 e1) s = div n0 n1 :!: s''
--                         where (n0 :!: s') = evalExp e0 s
--                               (n1 :!: s'') = evalExp e1 s'
-- evalExp (EAssgn _ _) s = undefined
-- evalExp (ESeq _ _) s = undefined
-- -- Binary
-- evalExp BTrue s = True :!: s
-- evalExp BFalse s = False :!: s
-- evalExp (Lt e0 e1) s = (n0 < n1) :!: s''
--                        where (n0 :!: s') = evalExp e0 s
--                              (n1 :!: s'') = evalExp e1 s'
-- evalExp (Gt e0 e1) s = (n0 > n1) :!: s''
--                        where (n0 :!: s') = evalExp e0 s
--                              (n1 :!: s'') = evalExp e1 s'
-- evalExp (Eq e0 e1) s = (n0 == n1) :!: s''
--                        where (n0 :!: s') = evalExp e0 s
--                              (n1 :!: s'') = evalExp e1 s'
-- evalExp (NEq e0 e1) s = (n0 /= n1) :!: s''
--                         where (n0 :!: s') = evalExp e0 s
--                               (n1 :!: s'') = evalExp e1 s'
-- evalExp (And p0 p1) s = (b0 && b1) :!: s''
--                         where (b0 :!: s') = evalExp p0 s
--                               (b1 :!: s'') = evalExp p1 s'
-- evalExp (Or p0 p1) s = (b0 || b1) :!: s''
--                         where (b0 :!: s') = evalExp p0 s
--                               (b1 :!: s'') = evalExp p1 s'
-- evalExp (Not p) s = not bv :!: s'
--                     where (bv :!: s') = evalExp p s
