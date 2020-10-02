module Eval3
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = (M.Map Variable Int, Integer)

-- Estado nulo
-- Completar la definición
initState :: State
initState = (M.empty, 0)

-- Busca el valor de una variable en un estado
-- Completar la definición
lookfor :: Variable -> State -> Either Error Int
lookfor v (s, _) = case s M.!? v of Nothing -> Left UndefVar
                                    Just nv -> Right nv

-- Cambia el valor de una variable en un estado
-- Completar la definición
update :: Variable -> Int -> State -> State
update x v (s, w) = (M.insert x v s, w)

-- Suma un costo dado al estado
-- Completar la definición
addWork :: Integer -> State -> State
addWork n = \(s, w) -> (s, w + n)

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
stepComm (Skip) s = return (Skip :!: s)
stepComm (Let v e) s = do (nv :!: s') <- evalExp e s
                          return (Skip :!: update v nv s')
stepComm (Seq Skip c1) s = return (c1 :!: s)
stepComm (Seq c0 c1) s = do (c0' :!: s') <- stepComm c0 s
                            return (Seq c0' c1 :!: s')
stepComm (IfThenElse b c0 c1) s = do (bv :!: s') <- evalExp b s
                                     return ((if bv then c0 else c1) :!: s')
stepComm w@(While b c) s = do (bv :!: s') <- evalExp b s
                              return ((if bv then Seq c w else Skip) :!: s')

-- Evalua una expresion
-- Completar la definición
evalExp :: Exp a -> State -> Either Error (Pair a State)
evalExp (Const nv) s = return (nv :!: s)
evalExp (Var v) s = do nv <- lookfor v s
                       return (nv :!: s)
evalExp (UMinus e) s = do (n :!: s') <- evalExp e s
                          return (-n :!: addWork 1 s')
evalExp (Plus e0 e1) s = do (nv0 :!: s') <- evalExp e0 s
                            (nv1 :!: s'') <- evalExp e1 s'
                            return (nv0 + nv1 :!: addWork 1 s'')
evalExp (Minus e0 e1) s = do (nv0 :!: s') <- evalExp e0 s
                             (nv1 :!: s'') <- evalExp e1 s'
                             return (nv0 - nv1 :!: addWork 1 s'')
evalExp (Times e0 e1) s = do (nv0 :!: s') <- evalExp e0 s
                             (nv1 :!: s'') <- evalExp e1 s'
                             return (nv0 * nv1 :!: addWork 2 s'')
evalExp (Div e0 e1) s = do (nv0 :!: s') <- evalExp e0 s
                           (nv1 :!: s'') <- evalExp e1 s'
                           case nv1 of 0 -> Left DivByZero
                                       _ -> return (div nv0 nv1 :!: addWork 2 s'')
evalExp (EAssgn _ _) s = undefined
evalExp (ESeq _ _) s = undefined
-- Binary
evalExp BTrue s = return (True :!: s)
evalExp BFalse s = return (False :!: s)
evalExp (Lt e0 e1) s = do (n0 :!: s') <- evalExp e0 s
                          (n1 :!: s'') <- evalExp e1 s'
                          return ((n0 < n1) :!: addWork 1 s'')
evalExp (Gt e0 e1) s = do (n0 :!: s') <- evalExp e0 s
                          (n1 :!: s'') <- evalExp e1 s'
                          return ((n0 > n1) :!: addWork 1 s'')
evalExp (Eq e0 e1) s = do (n0 :!: s') <- evalExp e0 s
                          (n1 :!: s'') <- evalExp e1 s'
                          return ((n0 == n1) :!: addWork 1 s'')
evalExp (NEq e0 e1) s = do (n0 :!: s') <- evalExp e0 s
                           (n1 :!: s'') <- evalExp e1 s'
                           return ((n0 /= n1) :!: addWork 1 s'')
evalExp (And p0 p1) s = do (n0 :!: s') <- evalExp p0 s
                           (n1 :!: s'') <- evalExp p1 s'
                           return ((n0 && n1) :!: addWork 1 s'')
evalExp (Or p0 p1) s = do (n0 :!: s') <- evalExp p0 s
                          (n1 :!: s'') <- evalExp p1 s'
                          return ((n0 || n1) :!: addWork 1 s'')
evalExp (Not p) s = do (bv :!: s') <- evalExp p s
                       return (not bv :!: addWork 1 s')
