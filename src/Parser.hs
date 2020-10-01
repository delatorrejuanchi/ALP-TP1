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
intexp = intseq

intseq :: Parser (Exp Int)
intseq = chainl1 intassgn (reservedOp lis "," >> return ESeq)

intassgn :: Parser (Exp Int)
intassgn = do v <- identifier lis
              reservedOp lis "="
              e <- intassgn
              return (EAssgn (Var v) e)
              <#> intterm

intterm :: Parser (Exp Int)
intterm = chainl1 intfactor ((reservedOp lis "+" >> return Plus) <#>
                             (reservedOp lis "-" >> return Minus))

intfactor :: Parser (Exp Int)
intfactor = chainl1 intatom ((reservedOp lis "*" >> return Times) <#>
                             (reservedOp lis "/" >> return Div))

intatom :: Parser (Exp Int)
intatom = parens lis intexp
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
boolexp = undefined

-----------------------------------
--- Parser de comandos
-----------------------------------

comm :: Parser Comm
comm = undefined

------------------------------------
-- Función de parseo
------------------------------------
parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)
