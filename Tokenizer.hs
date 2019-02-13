{-# LANGUAGE FlexibleInstances #-}

module Tokenizer where

import Data.List.Split
import Data.Char (toUpper, isSpace)
import Data.List
import Data.Maybe
import Text.Regex
import Text.Regex.Base

keywords :: [String]
keywords = ["alignas", "alignof", "and", "and_eq", "asm", "auto", "bitand", "bitor", "bool", "break",
            "case", "catch", "char", "char16_t", "char32_t", "class", "compl", "const", "constexpr", "const_cast",
            "continue", "decltype", "default", "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit",
            "export", "extern", "false", "float", "for", "friend", "goto", "if", "inline", "int", "long", "mutable",
            "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq", "private", "protected",
            "public", "register", "reinterpret_cast", "return", "short", "signed", "sizeof", "static", "static_assert",
            "static_cast", "struct", "switch", "template", "this", "thread_local", "throw", "true", "try", "typedef",
            "typeid", "typename", "union", "unsigned", "using", "virtual", "void", "volatile", "wchar_t", "while", "xor", "xor_eq"]
            
data Token = Ident String
           | KeyWord String
           | Number Int  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)

     
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
     
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
           
tokenize :: String -> [Token]
tokenize input = fmap tokenize' (splitOn " " (trim input)) where
    tokenize' token
             | elem token keywords = KeyWord token
             | matchTest (mkRegex "^[a-zA-Z][a-zA-Z_0-9]*$") token = Ident token
             | matchTest (mkRegex "^(0x|0X)((([0-9a-hA-H]|[0-9a-hA-H]'))+)([0-9a-hA-H])$") token = 
                Number $ parseHex $ filter (/= '\'') $ tail $ tail $ map toUpper token 
             | matchTest (mkRegex "^(0)((([0-7]|[0-7]'))+)([0-7])$") token = 
                Number $ parseOct $ filter (/= '\'') $ tail token 
             | matchTest (mkRegex "^(0b)((([01]|[01]'))+)([01])$") token = 
                Number $ parseBin $ filter (/= '\'') $ tail $ tail token 
             | matchTest (mkRegex "^[1-9]('[0-9]|[0-9])*$") token = 
                Number $ parseDec $ filter (/= '\'') token 
             | null token = error $ "empty input" 
             | otherwise = error $ "illegal token: " ++ token
