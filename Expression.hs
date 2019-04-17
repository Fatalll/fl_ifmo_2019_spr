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

calcParser :: Parser Char String (EAst Integer)
calcParser = do
  r <- por
  spaces
  eof
  return r

por = do
  f <- spaces *> pand
  s <- many $ spaces *> string "||" *> spaces *> pand
  return $ foldr1 (\f' expr -> BinOp Disj f' expr) (f : s)

pand = do
  f <- spaces *> pord
  s <- many $ spaces *> string "&&" *> spaces *> pord
  return $ foldr1 (\f' expr -> BinOp Conj f' expr) (f : s)

ps = do
  s <- string "==" <|> string "/=" <|> string "<=" <|> string ">=" <|> string "<" <|> string ">"
  case s of
    "==" -> return Eq
    "/=" -> return Neq
    "<=" -> return Le
    ">=" -> return Ge
    "<" -> return Lt
    ">" -> return Gt
    _ -> Combinators.fail "Incorrect symbol"

pord = do
  f <- spaces *> padd
  s <- try $ do
    spaces
    sign <- ps
    spaces
    s' <- padd
    return $ BinOp sign f s'
  
  case s of
    Nothing -> return f
    Just v -> return v

padd = do
  f <- spaces *> pmul
  s <- many $ do
    sign <- spaces *> (char '+' <|> char '-')
    s' <- spaces *> pmul
    return (sign, s')

  return $ foldl (\f' (sign, expr) -> BinOp (if sign == '+' then Sum else Minus) f' expr) f s

pmul = do
  f <- spaces *> ppow
  s <- many $ do
    sign <- spaces *> (char '*' <|> char '/')
    s' <- spaces *> ppow
    return (sign, s') 

  return $ foldl (\f' (sign, expr) -> BinOp (if sign == '*' then Mul else Div) f' expr) f s

ppow =
  do
    spaces
    char '('
    f <- spaces *> padd
    spaces
    char ')'
    s <- many $ do
      s' <- spaces *> char '^' *> spaces *> padd
      return s'
    return $ foldr1 (\f' expr -> BinOp Pow f' expr) (f : s)
  <|>
  do
    spaces
    char '('
    f <- spaces *> por
    spaces
    char ')'
    return f
  <|>
  do
    f <- spaces *> pnum
    spaces
    char '^'
    s <- spaces *> padd
    return $ BinOp Pow f s
  <|> pnum

pnum = do
  f <- spaces *> (digit <|> char '0')

  case f of
    '0' -> return $ Primary 0
    _ -> do
      s <- many $ (digit <|> char '0')
      return $ Primary (read (f : s) :: Integer)  


-- Change the signature if necessary
-- Constructs AST for the input expression
parseExpression :: String -> Either [String] (EAst Integer)
parseExpression input = case runParser calcParser (stream input) of
  Left e -> Left e
  Right (_, a) -> Right a

-- -- Change the signature if necessary
-- -- Calculates the value of the input expression
-- executeExpression :: String -> Either String Integer
-- executeExpression input = 
--   runParserUntilEof (expression undefined undefined) input

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

instance Show a => Show (EAst a) where
  show = show' 0
    where
      show' n t =
        (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
                  BinOp op l r -> printf "%s\n%s\n%s" (show op) (show' (ident n) l) (show' (ident n) r)
                  Primary x -> show x)
      ident = (+1)

test1 = parseExpression "1+3+4 >= 5 - 7 -10"
test2 = parseExpression "(10)^(1+2)^(5+5) > 3 && 1/2/3/4/5 <= 0"
test3 = parseExpression "((((((((125 > 5)))))))) || ((5^3)) < 6"

test_err = parseExpression "(10)^(1+2)^(5+5) > 3 && 1/2/3/4/5 <="
