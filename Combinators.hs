{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}

module Combinators where

import qualified Control.Monad.Fail as Fail
import           Data.Char
import           Prelude            hiding (fail, fmap, (<*>), (>>=))
import qualified Prelude

-- Parsing result is either an error message or some payload and a suffix of the input which is yet to be parsed
newtype Parser token err ok = Parser { runParser :: Stream token err -> Either [err] (Stream token err, ok) }

data Stream token err = Stream
  { tokens :: [token]
  , len    :: Int
  , still  :: Int
  , errors :: [err]
  } deriving (Show) 

stream :: [token] -> Stream token err
stream tokens = Stream tokens len len []
  where
    len = length tokens

streamS :: Stream token err -> (Maybe token, Stream token err)
streamS (Stream [] len _ errs) = (Nothing, Stream [] len 0 errs)
streamS (Stream (token:tokens) len still errs) =
  (Just token, Stream tokens len (still - 1) errs)

instance Functor (Parser s e) where
  fmap = fmap

instance Applicative (Parser s e) where
  pure = success
  (<*>) = (<*>)

instance Monad (Parser s String) where
  return = success
  (>>=) = (>>=)
  fail = fail 

instance Fail.MonadFail (Parser s String) where
  fail = fail

data Assoc = LAssoc -- left associativity
           | RAssoc -- right associativity
           | NAssoc -- not associative


-- Parser which always succeedes consuming no input
success :: ok -> Parser str err ok
success ok = Parser $ \s -> Right (s, ok)

-- Parser which fails no mater the input
fail :: err -> Parser str err ok
fail err = Parser $ \(Stream _ _ _ errs) -> Left (err : errs)

-- Biased choice: if the first parser succeedes, the second is never run
(<|>) :: Monoid err => Parser str err ok -> Parser str err ok -> Parser str err ok
p <|> q =
  Parser $ \s ->
  case runParser p s of
    Left errs ->
      case runParser q s of
        Left errs' -> Left $ errs <> errs'
        Right (Stream ts ln stl s'_errs, r) ->
          Right (Stream ts ln stl (errs <> s'_errs), r)
    x -> x

-- Monadic sequence combinator
(>>=) :: Parser str err a -> (a -> Parser str err b) -> Parser str err b
p >>= q =
  Parser $ \s ->
    case runParser p s of
      Right (s', result) -> (runParser $ q result) s'
      Left e             -> Left e

-- Applicative sequence combinator
(<*>) :: Parser str err (a -> b) -> Parser str err a -> Parser str err b
p <*> q =
  Parser $ \s ->
    case runParser p s of
      Right (s', f) ->
        case runParser q s' of
          Right (s'', result) -> Right (s'', f result)
          Left e              -> Left e
      Left e -> Left e

-- Applies a function to the parsing result, if parser succeedes
fmap :: (a -> b) -> Parser str err a -> Parser str err b
fmap f p =
  Parser $ \s ->
    case runParser p s of
      Right (s', a) -> Right (s', f a)
      Left e        -> Left e

-- Applies a parser once or more times
some :: Parser str err a -> Parser str err [a]
some p =
  Parser $ \s ->
    case runParser p s of
      Right (s'@(Stream ts ln st _), r) ->
        case (runParser $ some p) s' of
          Right (s'', rs) -> Right (s'', r : rs)
          Left new_errs   -> Right (Stream ts ln st new_errs, [r])
      Left e -> Left e

-- Applies a parser zero or more times
many :: Parser str err a -> Parser str err [a]
many p =
  Parser $ \s ->
    case runParser p s of
      Right (s'@(Stream ts ln st _), r) ->
        case (runParser $ many p) s' of
          Right (s'', rs) -> Right (s'', r : rs)
          Left errs       -> Right (Stream ts ln st errs, [r])
      Left err -> Right (s, [])

satisfy :: (Show token) => (token -> Bool) -> Parser token String token
satisfy f =
  Parser $ \s@(Stream _ _ _ errs) ->
    case s of
      (streamS -> (Just t, str))
        | f t -> Right (str, t)
        | otherwise -> Left $  [ (show $ len str - still str) ++ " pos, " ++ (show t) ++ " is not a token!" ] <> errs
      (streamS -> (Nothing, str)) ->
        Left $ [(show $ len str - still str) ++ " pos, Empty input!"] <> errs

try :: Monoid err => Parser Char err a -> Parser Char err (Maybe a)
try p = fmap Just p <|> success Nothing

-- Checks if the first element of the input is the given token
token :: (Eq token, Show token) => token -> Parser token String token
token t =
  Parser $ \s@(Stream _ _ _ errs) ->
    case s of
      (streamS -> (Just t', str))
        | t == t' -> Right (str, t)
        | otherwise -> Left $ [ (show $ len str - still str) ++ " pos, " ++ (show t') ++ " is not a token " ++ (show t) ++ "!"  ] <> errs
      _ -> Left $ ["Empty input!"] <> errs

-- Checks if the first character of the string is the one given
char :: Char -> Parser Char String Char
char = token

digit :: Parser Char String Char
digit = satisfy (\c -> isDigit c && c /= '0')

string :: String -> Parser Char String String
string = foldr (\a b -> pure (:) <*> char a <*> b) (pure [])

spaces :: Parser Char String [Char]
spaces = many $ satisfy isSpace 

eof :: Parser Char String Char
eof = Parser $ \s@(Stream _ _ _ errs) ->
    case s of
      (streamS -> (Nothing, str)) -> Right (str, '\0')
      (streamS -> (_, str)) ->
        Left $ [(show $ len str - still str) ++ " pos, Not the end of string!"] <> errs

-- General parser combinator for expressions
-- Binary operators are listed in the order of precedence (from lower to higher)
-- Binary operators on the same level of precedence have the same associativity
-- Binary operator is specified with a parser for the operator itself and a semantic function to apply to the operands
expression :: [(Assoc, [(Parser Char String b, a -> a -> a)])] -> 
              Parser Char String a ->
              Parser Char String a
expression opts primary = expr''
  where 
      expr' ((ass, (fp, fo) : aops) : ops) primary = 
        case ass of
          LAssoc -> do
            f <- spaces *> next
            s <- many $ do
              o <- spaces *> tp
              s' <- next
              return (o, s')

            return $ foldl (\f' (o, expr) -> o f' expr) f s

          RAssoc -> do
            f <- spaces *> next
            s <- many $ do
              o <- spaces *> tp
              s' <- next
              return (o, s')
            return $ snd $ foldr1 (\ (c, f') (o, s') -> (c, o f' s')) ((const, f) : s)
            
          NAssoc -> do
            f <- spaces *> next
            s <- try $ do
              spaces
              sign <- tp
              spaces
              s' <- next
              return $ sign f s'
            
            case s of
              Nothing -> return f
              Just v -> return v
        where next = expr' ops primary
              tp = foldr (\(p, o) r -> (p *> pure o) <|> r) (fp *> pure fo) aops

      expr' _ primary = primary <|> do
        spaces
        char '('
        f <- spaces *> expr' opts primary
        spaces
        char ')'
        return f

      expr'' = expr' opts primary
              

-- runParserUntilEof :: Foldable t => Parser (t str) String ok -> (t str) -> Either [String] ok 
-- runParserUntilEof p inp = 
--   either (Left . id) (\(rest, ok) -> if null rest then Right ok else Left ["Expected eof"]) (runParser p (stream inp))
