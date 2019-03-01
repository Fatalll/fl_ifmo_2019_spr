module Combinators where

import qualified Prelude
import Data.Char
import Prelude hiding (fail, fmap, (<*>), (>>=))

data Trie a = Trie { following :: [(a, Trie a)], isWord :: Bool }

insert :: String -> Trie Char -> Trie Char
insert (c : word) trie = case lookup c $ following trie of
  Just _ -> Trie (update <$> following trie) (isWord trie) where
    update (c', t) = if c == c' then (c, insert word t)
                     else (c', t)
  _ -> Trie ((c, insert word (Trie [] False)) : following trie) (isWord trie)
insert _ trie = Trie (following trie) True

-- Parsing result is some payload and a suffix of the input which is yet to be parsed
newtype Parser str ok = Parser { runParser :: str -> Maybe (str, ok) }

instance Functor (Parser s) where
  fmap = fmap

instance Applicative (Parser s) where
  pure = success
  (<*>) = (<*>)

instance Monad (Parser s) where
  return = success
  (>>=) = (>>=)
  fail = const fail

-- Parser which always succeedes consuming no input
success :: ok -> Parser str ok
success ok = Parser $ \s -> Just (s, ok)

-- Parser which fails no mater the input
fail :: Parser str ok
fail = Parser $ const Nothing

-- Biased choice: if the first parser succeedes, the second is never run
(<|>) :: Parser str ok -> Parser str ok -> Parser str ok
p <|> q = Parser $ \s ->
  case runParser p s of
    Nothing -> runParser q s
    x -> x

-- Default sequence combinator
-- If the first parser succeedes then the second parser is used
-- If the first does not succeed then the second one is never tried
-- The result is collected into a pair
seq :: Parser str a -> Parser str b -> Parser str (a, b)
p `seq` q = Parser $ \s ->
  case runParser p s of
    Just (s', p_result) -> case runParser q s' of
      Just (s'', q_result) -> Just (s'', (p_result, q_result))
      _ -> Nothing
    _ -> Nothing

-- Monadic sequence combinator
(>>=) :: Parser str a -> (a -> Parser str b) -> Parser str b
p >>= q = Parser $ \s ->
  case runParser p s of
    Just (s', result) -> (runParser $ q result) s'
    _ -> Nothing

-- Applicative sequence combinator
(<*>) :: Parser str (a -> b) -> Parser str a -> Parser str b
p <*> q = Parser $ \s ->
  case runParser p s of
    Just (s', f) -> case runParser q s' of
      Just (s'', result) -> Just (s'', f result)
      _ -> Nothing
    _ -> Nothing

-- Applies a function to the parsing result, if parser succeedes
fmap :: (a -> b) -> Parser str a -> Parser str b
fmap f p = Parser $ \s ->
  case runParser p s of
    Just (s', a) -> Just (s', f a)
    _ -> Nothing

-- Applies a parser once or more times
some :: Parser str a -> Parser str [a]
some p = Parser $ \s ->
  case runParser p s of
    Just (s', r) -> case (runParser $ some p) s' of
      Just (s'', rs) -> Just (s'', r : rs)
      _ -> Just (s', [r])
    _ -> Nothing

-- Applies a parser zero or more times
many :: Parser str a -> Parser str [a]
many p = Parser $ \s ->
  case runParser p s of
    Just (s', r) -> case (runParser $ many p) s' of
      Just (s'', rs) -> Just (s'', r : rs)
      _ -> Just (s', [r])
    _ -> Just (s, [])

-- Parses keywords
keywords :: [String] -> Parser String String
keywords kws = Parser $ \s ->
  next s trie [] where
    trie = foldr insert (Trie [] False) kws
    next input@(' ' : _) (Trie _ True) ok = Just (input, ok)
    next (' ' : _) (Trie _ False) ok = Nothing
    next (c : str) (Trie f_ing _) ok = case lookup c f_ing of
      Just t -> next str t (ok ++ [c])
      _ -> Nothing
    next [] (Trie _ True) ok = Just ([], ok)
    next [] _ _ = Nothing

-- Checks if the first element of the input is the given token
token :: Eq token => token -> Parser [token] token
token t = Parser $ \s ->
  case s of
    (t' : s') | t == t' -> Just (s', t)
    _ -> Nothing

satisfy :: (token -> Bool) -> Parser [token] token
satisfy f = Parser $ \s ->
  case s of
    (t : s') | f t -> Just (s', t)
    _ -> Nothing

-- Checks if the first character of the string is the one given
char :: Char -> Parser String Char
char = token

stringEof :: Parser String Char
stringEof = Parser $ \s ->
  case s of
    [] -> Just ([], '\0')
    _  -> Nothing

zeroOne :: Eq token => token -> Parser [token] token
zeroOne t = Parser $ \s ->
  case s of
    (t' : s') | t == t' -> Just (s', t)
    _ -> Just (s, t)

try :: Parser String a -> Parser String (Maybe a)
try p = fmap Just p <|> success Nothing

keywords_satisfy :: [String] -> (Char -> Bool) -> Parser String String
keywords_satisfy kws p = Parser $ \s ->
  next s trie [] where
    trie = foldr insert (Trie [] False) kws
    next input@(c : str) (Trie f_ing s) ok
            | p c = if s then Just (input, ok) else Nothing
            | otherwise = case lookup c f_ing of
                Just t -> next str t (ok ++ [c])
                _ -> Nothing
    next [] (Trie _ True) ok = Just ([], ok)
    next [] _ _ = Nothing

parseList :: Parser String el -> Parser String d -> Parser String lbr
  -> Parser String rbr -> (Int -> Bool) -> Parser String [el]
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
      if p $ length elms + 1 then return (v : elms) else fail
