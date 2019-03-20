{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}

module Combinators where

import qualified Control.Monad.Fail as Fail
import           Data.Char
import           Prelude            hiding (fail, fmap, (<*>), (>>=))
import qualified Prelude

-- Parsing result is either an error message or some payload and a suffix of the input which is yet to be parsed
newtype Parser token err ok = Parser
  { runParser :: Stream token err -> Either [err] (Stream token err, ok)
  }

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

-- streamS :: token -> Stream token -> Stream token
-- streamS token (Stream tokens all curr) = Stream (token : tokens) all (curr + 1)
data Trie a = Trie
  { following :: [(a, Trie a)]
  , isWord    :: Bool
  }

insert :: String -> Trie Char -> Trie Char
insert (c:word) trie =
  case lookup c $ following trie of
    Just _ -> Trie (update <$> following trie) (isWord trie)
      where update (c', t) =
              if c == c'
                then (c, insert word t)
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

instance Fail.MonadFail (Parser s String) where
  fail = fail

-- Parser which always succeedes consuming no input
success :: ok -> Parser str err ok
success ok = Parser $ \s -> Right (s, ok)

-- Parser which fails no mater the input
fail :: err -> Parser str err ok
fail err = Parser $ \(Stream _ _ _ errs) -> Left (err : errs)

-- ((show $ ln str - st str) ++ " pos, " ++ err)
-- Biased choice: if the first parser succeedes, the second is never run
(<|>) ::
     Monoid err => Parser str err ok -> Parser str err ok -> Parser str err ok
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

-- Checks if the first element of the input is the given token
token :: (Eq token, Show token) => token -> Parser token String token
token t =
  Parser $ \s@(Stream _ _ _ errs) ->
    case s of
      (streamS -> (Just t', str))
        | t == t' -> Right (str, t)
        | otherwise ->
          Left $
          [ (show $ len str - still str) ++
            " pos, " ++ (show t') ++ " is not a token!"
          ] <>
          errs
      _ -> Left $ ["Empty input!"] <> errs

satisfy :: (Show token) => (token -> Bool) -> Parser token String token
satisfy f =
  Parser $ \s@(Stream _ _ _ errs) ->
    case s of
      (streamS -> (Just t, str))
        | f t -> Right (str, t)
        | otherwise ->
          Left $
          [ (show $ len str - still str) ++
            " pos, " ++ (show t) ++ " is not a token!"
          ] <>
          errs
      (streamS -> (Nothing, str)) ->
        Left $ [(show $ len str - still str) ++ " pos, Empty input!"] <> errs

-- Checks if the first character of the string is the one given
char :: Char -> Parser Char String Char
char = token

stringEof :: Parser Char String Char
stringEof =
  Parser $ \s@(Stream _ _ _ errs) ->
    case s of
      (streamS -> (Nothing, str)) -> Right (str, '\0')
      (streamS -> (_, str)) ->
        Left $
        [(show $ len str - still str) ++ " pos, Not the end of string!"] <> errs

-- zeroOne :: Eq token => token -> Parser token err token
-- zeroOne t =
--   Parser $ \s ->
--     case s of
--       (streamS -> (Just t', str))
--         | t == t' -> Right (str, t)
--       _ -> Right (s, t)
try :: Monoid err => Parser Char err a -> Parser Char err (Maybe a)
try p = fmap Just p <|> success Nothing

keywords_satisfy :: [String] -> (Char -> Bool) -> Parser Char String String
keywords_satisfy kws p = Parser $ \s -> next s trie []
  where
    trie = foldr insert (Trie [] False) kws
    next input@(streamS -> (t, str@(Stream _ _ _ errs))) (Trie f_ing s) ok =
      case t of
        Nothing ->
          if s
            then Right (input, ok)
            else Left $ ["Empty trie!"] <> errs
        Just t
          | p t ->
            if s
              then Right (input, ok)
              else Left $
                   [ (show $ len str - still str) ++
                     " pos, " ++ ok ++ " is not a token!"
                   ] <>
                   errs
          | otherwise ->
            case lookup t f_ing of
              Just t' -> next str t' (ok ++ [t])
              _ ->
                Left $
                [ (show $ len str - still str) ++
                  " pos, " ++ ok ++ [t] ++ " is not a token!"
                ] <>
                errs

parseList ::
     Parser Char String el
  -> Parser Char String d
  -> Parser Char String lbr
  -> Parser Char String rbr
  -> (Int -> Bool)
  -> Parser Char String [el]
parseList el delim lbr rbr p = do
  let skipSpaces = many $ satisfy isSpace
  skipSpaces
  lbr
  skipSpaces
  first <- try el
  elms <- many $ (delim >> skipSpaces) *> el <* skipSpaces
  rbr
  case first of
    Nothing -> do
      if p $ 0
        then return []
        else fail "Empty input!"
    Just v -> do
      if p $ length elms + 1
        then return (v : elms)
        else fail "Incorrect number of elements"

test_position =
  (runParser $
   (keywords_satisfy ["abc"] (\c -> c == ' ')) <|>
   (keywords_satisfy ["abcde"] (\c -> c == ' ')))
    (stream "abcdef")
