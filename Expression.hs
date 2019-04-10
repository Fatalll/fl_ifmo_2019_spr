module Expression where

import Text.Printf
import Combinators

data Operator = Pow
              | Mul
              | Div
              | Sum
              | Minus
              | Eq
              | Neq
              | Le
              | Lt
              | Ge
              | Gt
              | Conj
              | Disj

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a = BinOp Operator (EAst a) (EAst a)
            | Primary a

pnum = do
  f <- spaces *> (digit <|> char '0')

  case f of
    '0' -> return $ Primary 0
    _ -> do
      s <- many $ (digit <|> char '0')
      return $ Primary (read (f : s) :: Integer)  

calcPnum = do
  f <- spaces *> (digit <|> char '0')

  case f of
    '0' -> return 0
    _ -> do
      s <- many $ (digit <|> char '0')
      return (read (f : s) :: Integer)  
            

parserUntilEof :: Parser Char String a -> Parser Char String a
parserUntilEof p = do
  r <- p
  spaces
  eof
  return r

-- Change the signature if necessary
-- Constructs AST for the input expression
parseExpression :: String -> Either String (EAst Integer)
parseExpression input = case runParser (parserUntilEof $ expression specification pnum) (stream input) of
  Left e -> Left $ concat e
  Right (_, a) -> Right a

-- Change the signature if necessary
-- Calculates the value of the input expression
executeExpression :: String -> Either String Integer
executeExpression input = case runParser (parserUntilEof $ expression calcSpecification calcPnum) (stream input) of
  Left e -> Left $ concat e
  Right (_, a) -> Right a

instance Show Operator where
  show Pow   = "^"
  show Mul   = "*"
  show Div   = "/"
  show Sum   = "+"
  show Minus = "-"
  show Eq    = "=="
  show Neq   = "/="
  show Le    = "<="
  show Lt    = "<"
  show Ge    = ">="
  show Gt    = ">"
  show Conj  = "&&"
  show Disj  = "||"

specification =
  [
    (RAssoc, [ (string "||", BinOp Disj) ]),
    (RAssoc, [ (string "&&", BinOp Conj) ]),
    (NAssoc, [ (string "==", BinOp Eq), (string "/=", BinOp Neq), (string "<=", BinOp Le), (string ">=", BinOp Ge), (string "<", BinOp Lt), (string ">", BinOp Gt) ]),
    (LAssoc, [ (string "+", BinOp Sum), (string "-", BinOp Minus) ]),
    (LAssoc, [ (string "*", BinOp Mul), (string "/", BinOp Div) ]),
    (RAssoc, [ (string "^", BinOp Pow) ])
  ]

calcSpecification =
  [
    (RAssoc, [ (string "||", (\f s -> if f /= 0 || s /= 0 then 1 else 0)) ]),
    (RAssoc, [ (string "&&", (\f s -> if f /= 0 && s /= 0 then 1 else 0)) ]),
    (NAssoc, [ (string "==", (\f s -> if f == s then 1 else 0)),
               (string "/=", (\f s -> if f /= s then 1 else 0)),
               (string "<=", (\f s -> if f <= s then 1 else 0)),
               (string ">=", (\f s -> if f >= s then 1 else 0)),
               (string "<",  (\f s -> if f < s then 1 else 0)),
               (string ">",  (\f s -> if f > s then 1 else 0)) ]),
    (LAssoc, [ (string "+", (+)), (string "-", (-)) ]),
    (LAssoc, [ (string "*", (*)), (string "/", div) ]),
    (RAssoc, [ (string "^", (^)) ])
  ]

instance Show a => Show (EAst a) where
  show = show' 0
    where
      show' n t =
        (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
                  BinOp op l r -> printf "%s\n%s\n%s" (show op) (show' (ident n) l) (show' (ident n) r)
                  Primary x -> show x)
      ident = (+1)

test1 = executeExpression "22 > 3"
test2 = executeExpression "2 + 2 * 2"
test3 = executeExpression "2^2^3"
test4 = executeExpression "3 > 2 && 5 < 7"