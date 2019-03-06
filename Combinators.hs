{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Combinators where

import qualified Prelude
import Data.Char
import Prelude hiding (fail, fmap, (<*>), (>>=))

-- Parsing result is either an error message or some payload and a suffix of the input which is yet to be parsed
newtype Parser str err ok = Parser { runParser :: str -> Either err (str, ok) }

data Trie a = Trie { following :: [(a, Trie a)], isWord :: Bool }

insert :: String -> Trie Char -> Trie Char
insert (c : word) trie = case lookup c $ following trie of
  Just _ -> Trie (update <$> following trie) (isWord trie) where
    update (c', t) = if c == c' then (c, insert word t)
                     else (c', t)
  _ -> Trie ((c, insert word (Trie [] False)) : following trie) (isWord trie)
insert _ trie = Trie (following trie) True

instance Functor (Parser s e) where
  fmap = fmap

instance Applicative (Parser s e) where
  pure = success
  (<*>) = (<*>)

instance Monad (Parser s String) where
  return = success
  (>>=) = (>>=)
  fail = fail

-- Parser which always succeedes consuming no input
success :: ok -> Parser str err ok
success ok = Parser $ \s -> Right (s, ok)

-- Parser which fails no mater the input
fail :: err -> Parser str err ok 
fail err = Parser $ \s -> Left err

-- Biased choice: if the first parser succeedes, the second is never run
(<|>) :: Monoid err => Parser str err ok -> Parser str err ok -> Parser str err ok 
p <|> q = Parser $ \s ->
  case runParser p s of
    Left err -> case runParser q s of
      Left err -> Left $ err <> err
      x -> x
    x -> x

-- -- Default sequence combinator
-- -- If the first parser succeedes then the second parser is used
-- -- If the first does not succeed then the second one is never tried
-- -- The result is collected into a pair
-- seq :: Parser str a -> Parser str b -> Parser str (a, b)
-- p `seq` q = Parser $ \s ->
--   case runParser p s of
--     Just (s', p_result) -> case runParser q s' of
--       Just (s'', q_result) -> Just (s'', (p_result, q_result))
--       _ -> Nothing
--     _ -> Nothing

-- Monadic sequence combinator
(>>=) :: Parser str err a -> (a -> Parser str err b) -> Parser str err b 
p >>= q = Parser $ \s ->
  case runParser p s of
    Right (s', result) -> (runParser $ q result) s'
    Left e -> Left e

-- Applicative sequence combinator
(<*>) :: Parser str err (a -> b) -> Parser str err a -> Parser str err b 
p <*> q = Parser $ \s ->
  case runParser p s of
    Right (s', f) -> case runParser q s' of
      Right (s'', result) -> Right (s'', f result)
      Left e -> Left e
    Left e -> Left e

-- Applies a function to the parsing result, if parser succeedes
fmap :: (a -> b) -> Parser str err a -> Parser str err b
fmap f p = Parser $ \s ->
  case runParser p s of
    Right (s', a) -> Right (s', f a)
    Left e -> Left e

-- Applies a parser once or more times
some :: Parser str err a -> Parser str err [a] 
some p = Parser $ \s ->
  case runParser p s of
    Right (s', r) -> case (runParser $ some p) s' of
      Right (s'', rs) -> Right (s'', r : rs)
      _ -> Right (s', [r])
    Left e -> Left e

-- Applies a parser zero or more times
many :: Parser str err a -> Parser str err [a]
many p = Parser $ \s ->
  case runParser p s of
    Right (s', r) -> case (runParser $ many p) s' of
      Right (s'', rs) -> Right (s'', r : rs)
      _ -> Right (s', [r])
    _ -> Right (s, [])

-- Parses keywords
keywords :: [String] -> Parser String String String
keywords kws = Parser $ \s ->
  next s trie [] where
    trie = foldr insert (Trie [] False) kws
    next input@(' ' : _) (Trie _ True) ok = Right (input, ok)
    next (' ' : _) (Trie _ False) ok = Left $ ok ++ " is not a token!"
    next (c : str) (Trie f_ing _) ok = case lookup c f_ing of
      Just t -> next str t (ok ++ [c])
      _ -> Left $ ok ++ [c] ++ " is not a token!"
    next [] (Trie _ True) ok = Right ([], ok)
    next [] _ _ = Left "Empty trie!"

-- Checks if the first element of the input is the given token
token :: (Eq token, Show token) => token -> Parser [token] String token 
token t = Parser $ \s ->
  case s of
    (t' : s') | t == t' -> Right (s', t)
    t' -> Left $ (show t') ++ " is not a token!"

satisfy :: (Show token) => (token -> Bool) -> Parser [token] String token
satisfy f = Parser $ \s ->
  case s of
    (t : s') | f t -> Right (s', t)
    t' -> Left $ (show t') ++ " is not a token!"

-- Checks if the first character of the string is the one given
char :: Char -> Parser String String Char
char = token

stringEof :: Parser String String Char 
stringEof = Parser $ \s ->
  case s of
    [] -> Right ([], '\0')
    _  -> Left "Not the end of string!"

zeroOne :: Eq token => token -> Parser [token] err token 
zeroOne t = Parser $ \s ->
  case s of
    (t' : s') | t == t' -> Right (s', t)
    _ -> Right (s, t)

try :: Monoid err => Parser String err a -> Parser String err (Maybe a) 
try p = fmap Just p <|> success Nothing

keywords_satisfy :: [String] -> (Char -> Bool) -> Parser String String String
keywords_satisfy kws p = Parser $ \s ->
  next s trie [] where
    trie = foldr insert (Trie [] False) kws
    next input@(c : str) (Trie f_ing s) ok
            | p c = if s then Right (input, ok) else Left $ ok ++ " is not a token!"
            | otherwise = case lookup c f_ing of
                Just t -> next str t (ok ++ [c])
                _ -> Left $ ok ++ [c] ++ " is not a token!"
    next [] (Trie _ True) ok = Right ([], ok)
    next [] _ _ = Left "Empty trie!"

parseList :: Parser String String el -> Parser String String d -> Parser String String lbr 
  -> Parser String String rbr -> (Int -> Bool) -> Parser String String [el]
parseList el delim lbr rbr p = do
  let skipSpaces = many $ satisfy isSpace
  skipSpaces
  lbr
  skipSpaces
  first <- try el
  skipSpaces

  case first of
    Nothing -> do
      rbr
      return []
    Just v -> do
      elms <- many $ (delim >> skipSpaces) *> el <* skipSpaces
      rbr
      if p $ length elms + 1 then return (v : elms) else fail "sds"
