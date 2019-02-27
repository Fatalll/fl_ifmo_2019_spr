module Automaton where

import qualified Data.Map.Lazy as Maps
import qualified Data.Set as Set
import Data.Char

import Combinators

type Set = Set.Set
type Map = Maps.Map

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: Map (q, s) (Maybe q)
                               } deriving Show

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
-- Pick appropriate types for s and q

parserAutomaton = do
  let skipSpaces = many $ satisfy isSpace
  
  alphabet <- parseList (many (satisfy isLetter)) (char ',') (char '<') (char '>') (> 0)
  skipSpaces
  states <- parseList (many ((satisfy isLetter) <|> (satisfy isDigit) <|> (char '_')))
    (char ',') (char '<') (char '>') (> 0)
  skipSpaces
  inits <- parseList (keywords_satisfy states (\c -> c == ',' || c == '>' || c == ' '))
   (char ',') (char '<') (char '>') (== 1)
  skipSpaces
  terms <- parseList (keywords_satisfy states (\c -> c == ',' || c == '>' || c == ' '))
   (char ',') (char '<') (char '>') (> 0)
  skipSpaces
  let triplets = do {
    [begin, sigma, end] <- parseList (many ((satisfy isLetter) <|> (satisfy isDigit) <|> (char '_')))
     (char ',') (char '(') (char ')') (== 3);
    if elem begin states;
      then if elem end states;
        then if elem sigma alphabet;
          then return ((begin, sigma), Just end);
          else Combinators.fail;
        else Combinators.fail;
      else Combinators.fail;
  }

  delts <- parseList triplets (char ',') (char '<') (char '>') (>= 0)

  return $ Automaton (Set.fromList alphabet) (Set.fromList states) (head inits) (Set.fromList terms) (Maps.fromList delts)

parseAutomaton :: String -> Maybe (Automaton String String)
parseAutomaton input = snd <$> (runParser $ parserAutomaton) input

test = parseAutomaton "<a,b,c> <1,2,3> <1> <3> <(1,a,2),(2,b,3),(3,c,1)>"
