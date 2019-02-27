module Tokenizer where

import Combinators
import Data.Char
import Data.Maybe
import Data.List
import Prelude hiding (fail, fmap, (<*>), (>>=))

data Token = Ident String
           | KeyWord String
           | Number Int  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)

keywords_list :: [String]
keywords_list =
  ["alignas", "alignof", "and", "and_eq", "asm", "auto", "bitand", "bitor", "bool", "break",
   "case", "catch", "char", "char16_t", "char32_t", "class", "compl", "const", "constexpr", "const_cast",
   "continue", "decltype", "default", "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit",
   "export", "extern", "false", "float", "for", "friend", "goto", "if", "inline", "int", "long", "mutable",
   "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private", "protected",
   "public", "register", "reinterpret_cast", "return", "short", "signed", "sizeof", "static", "static_assert",
   "static_cast", "struct", "switch", "template", "this", "thread_local", "throw", "true", "try", "typedef",
   "typeid", "typename", "union", "unsigned", "using", "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq"]

tokenize :: String -> [Token]
tokenize input = case (runParser $ many ((many $ char ' ') >>
  (KeyWord <$> parseKeyWord) <|> (Number <$> parseNumber) <|> (Ident <$> parseIdent))) input of
    Just (_, tokens) -> tokens
    _ -> []

parseIdent :: Parser String String
parseIdent = do
  first_letter <- satisfy isLetter
  others <- many ((satisfy isLetter) <|> (satisfy isDigit) <|> (char '_'))
  return (first_letter : others)

parseKeyWord :: Parser String String
parseKeyWord = keywords keywords_list

parseNumber :: Parser String Int
parseNumber = do
  first <- satisfy isDigit
  case first of
    '0' -> do
      second <- satisfy (`elem` "xb") <|> (satisfy isOctDigit) <|> stringEof
      case second of
        'x' -> do
          others <- some $ (satisfy isHexDigit <|> (zeroOne '\'' >> (satisfy isHexDigit)))
          return $ parseHex others
        'b' -> do
          others <- some $ (satisfy (`elem` "01") <|> (zeroOne '\'' >> (satisfy (`elem` "01"))))
          return $ parseBin others
        '\0' -> return 0
        otherwise -> do
          others <- many $ (satisfy isOctDigit <|> (zeroOne '\'' >> (satisfy isOctDigit)))
          return $ parseOct (second : others)
    otherwise -> do
      others <- many $ (satisfy isDigit <|> (zeroOne '\'' >> (satisfy isDigit)))
      return $ parseDec (first : others)


hexChar :: Char -> Int
hexChar ch = fromMaybe (error $ "illegal char " ++ [ch]) $
  elemIndex ch "0123456789ABCDEF"

parseHex :: Foldable t => t Char -> Int
parseHex hex = foldl' f 0 hex where
  f n c = 16 * n + hexChar c

octChar :: Char -> Int
octChar ch = fromMaybe (error $ "illegal char " ++ [ch]) $
    elemIndex ch "01234567"

parseOct :: Foldable t => t Char -> Int
parseOct oct = foldl' f 0 oct where
    f n c = 8 * n + octChar c

binChar :: Char -> Int
binChar ch = fromMaybe (error $ "illegal char " ++ [ch]) $
    elemIndex ch "01"

parseBin :: Foldable t => t Char -> Int
parseBin bin = foldl' f 0 bin where
    f n c = 2 * n + binChar c

decChar :: Char -> Int
decChar ch = fromMaybe (error $ "illegal char " ++ [ch]) $
    elemIndex ch "0123456789"

parseDec :: Foldable t => t Char -> Int
parseDec dec = foldl' f 0 dec where
    f n c = 10 * n + decChar c
