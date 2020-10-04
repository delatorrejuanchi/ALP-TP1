module AST where

{- Sintaxis Abstracta:
intexp ::= intseq
intseq ::= intassgn | intseq , intassgn
intassgn ::= intterm | var = intassgn
intterm ::= intterm +- intfactor
intfactor ::= intfactor */ intatom
intatom ::= nat | var | -intexp           | (intexp) no sé si va en la abstracta el tema de los parentesis?????????

boolexp ::= boolor

boolor  ::= booland
        ::= booland ∨ booland

booland ::= boolneg
        ::= boolneg ∧ boolneg

boolneg ::= boolcomp
        ::= ¬ booolcomp

boolcomp ::= true | false
         ::= intexp (== | != | < | >) intexp

comm' ::= comm'; comm
comm ::= skip
        | var = intexp
        | if boolexp then comm else comm
        | while boolexp do comm

intexp ::= nat | var | −u intexp
          | intexp + intexp
          | intexp −b intexp
          | intexp × intexp
          | intexp ÷ intexp
          | var = intexp
          | intexp , intexp
boolexp ::= true | false
          | intexp == intexp
          | intexp != intexp
          | intexp < intexp
          | intexp > intexp
          | boolexp ∧ boolexp
          | boolexp ∨ boolexp
          | ¬ boolexp
comm ::= skip
        | var = intexp
        | comm; comm
        | if boolexp then comm else comm
        | while boolexp do comm

   Sintaxis Concreta:
digit ::= ’0’ | ’1’ | · · · | ’9’
letter ::= ’a’ | · · · | ’Z’
nat ::= digit | digit nat
var ::= letter | letter var
intexp ::= nat
          | var
          | ’-’ intexp
          | intexp ’+’ intexp
          | intexp ’-’ intexp
          | intexp ’*’ intexp
          | intexp ’/’ intexp
          | ’(’ intexp ’)’
boolexp ::= ’true’ | ’false’
          | intexp ’==’ intexp
          | intexp ’!=’ intexp
          | intexp ’<’ intexp
          | intexp ’>’ intexp
          | boolexp ’&&’ boolexp
          | boolexp ’||’ boolexp
          | ’!’ boolexp
          | ’(’ boolexp ’)’
comm ::= skip
        | var ’=’ intexp
        | comm ’;’ comm
        | ’if’ boolexp ’{’ comm ’}’
        | ’if’ boolexp ’{’ comm ’}’ ’else’ ’{’ comm ’}’
        | ’while’ boolexp ’{’ comm ’}’
-}

-- Identificadores de Variable
type Variable = String

-- Expresiones, aritmeticas y booleanas
data Exp a where
  -- Expresiones enteras
  Const ::Int -> Exp Int
  Var ::Variable -> Exp Int
  UMinus ::Exp Int -> Exp Int
  Plus ::Exp Int -> Exp Int -> Exp Int
  Minus ::Exp Int -> Exp Int -> Exp Int
  Times ::Exp Int -> Exp Int -> Exp Int
  Div ::Exp Int -> Exp Int -> Exp Int
  EAssgn ::Variable -> Exp Int -> Exp Int
  ESeq ::Exp Int -> Exp Int -> Exp Int
  -- Expreseiones booleanas
  BTrue ::Exp Bool
  BFalse ::Exp Bool
  Lt ::Exp Int -> Exp Int -> Exp Bool
  Gt ::Exp Int -> Exp Int -> Exp Bool
  And ::Exp Bool -> Exp Bool -> Exp Bool
  Or ::Exp Bool -> Exp Bool -> Exp Bool
  Not ::Exp Bool -> Exp Bool
  Eq ::Exp Int -> Exp Int -> Exp Bool
  NEq ::Exp Int -> Exp Int -> Exp Bool

deriving instance Show (Exp a)
deriving instance Eq (Exp a)

-- Comandos (sentencias)
-- Observar que solo se permiten variables de un tipo (entero)
data Comm
  = Skip
  | Let Variable (Exp Int)
  | Seq Comm Comm
  | IfThenElse (Exp Bool) Comm Comm
  | While (Exp Bool) Comm
  deriving (Show, Eq)

pattern IfThen :: Exp Bool -> Comm -> Comm
pattern IfThen b c = IfThenElse b c Skip

data Error = DivByZero | UndefVar deriving (Eq, Show)
