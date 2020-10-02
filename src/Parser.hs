module Parser where

import           AST
import           Text.Parsec.Language          (emptyDef)
import           Text.Parsec.Token
import           Text.ParserCombinators.Parsec

-----------------------
-- Funcion para facilitar el testing del parser.
totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t

-- Analizador de Tokens
lis :: TokenParser u
lis = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "else", "while", "skip"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        ]
    }
  )

(<#>) :: Parser a -> Parser a -> Parser a
p <#> q = try p <|> q

----------------------------------
--- Parser de expressiones enteras
-----------------------------------
intexp :: Parser (Exp Int)
intexp = intterm

intseq :: Parser (Exp Int)
intseq = chainl1 intassgn (reservedOp lis "," >> return ESeq)

intassgn :: Parser (Exp Int)
intassgn = do v <- identifier lis
              reservedOp lis "="
              e <- intassgn
              return (EAssgn v e)
              <#> intterm

intterm :: Parser (Exp Int)
intterm = chainl1 intfactor ((reservedOp lis "+" >> return Plus) <#>
                             (reservedOp lis "-" >> return Minus))

intfactor :: Parser (Exp Int)
intfactor = chainl1 intatom ((reservedOp lis "*" >> return Times) <#>
                             (reservedOp lis "/" >> return Div))

intatom :: Parser (Exp Int)
intatom = parens lis intterm
          <#>
          do reservedOp lis "-"
             n <- intatom
             return (UMinus n)
          <#>
          do n <- integer lis
             return (Const (fromIntegral n))
          <#>
          do v <- identifier lis
             return (Var v)

-----------------------------------
--- Parser de expressiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = boolor

boolor :: Parser (Exp Bool)
boolor = chainl1 booland (reservedOp lis "||" >> return Or)

booland :: Parser (Exp Bool)
booland = chainl1 boolatom (reservedOp lis "&&" >> return And)

boolatom :: Parser (Exp Bool)
boolatom = parens lis boolexp
          <#>
          do reservedOp lis "!"
             b <- boolexp
             return (Not b)
          <#>
          (reserved lis "true" >> return BTrue)
          <#>
          (reserved lis "false" >> return BFalse)
          <#>
          do e1 <- intexp
             comp <- boolcomp
             e2 <- intexp
             return (comp e1 e2)

boolcomp :: Parser (Exp Int -> Exp Int -> Exp Bool)
boolcomp = (reservedOp lis "<" >> return Lt) <#>
           (reservedOp lis ">" >> return Gt) <#>
           (reservedOp lis "==" >> return Eq) <#>
           (reservedOp lis "!=" >> return NEq)

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = commseq

commseq :: Parser Comm
commseq = chainl1 commatom (reservedOp lis ";" >> return Seq)

commatom :: Parser Comm
commatom = (reserved lis "skip" >> return Skip)
          <#>
          do var <- identifier lis
             reservedOp lis "="
             e1 <- intexp
             return (Let var e1)
          <#>
          do reserved lis "if"
             b <- boolexp
             c1 <- braces lis comm
             do reserved lis "else"
                c2 <- braces lis comm
                return (IfThenElse b c1 c2)
                <#>
                return (IfThen b c1)
          <#>
          do reserved lis "while"
             b <- boolexp
             c <- braces lis comm
             return (While b c)


------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
