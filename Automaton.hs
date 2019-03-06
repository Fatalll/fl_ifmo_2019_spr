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

-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: Automaton a b -> Bool
isDFA = undefined

-- Checks if the automaton is nondeterministic (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Automaton a b -> Bool
isNFA = undefined

-- Checks if the automaton is complete (there exists a transition for each state and each input symbol)
isComplete :: Automaton a b -> Bool 
isComplete = undefined

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: Automaton a b -> Bool
isMinimal = undefined

parserAutomaton = do
  let skipSpaces = many $ satisfy isSpace

  alphabet <- parseList (many ((satisfy isLetter) <|> (satisfy isDigit) <|> (char '_'))) (char ',') (char '<') (char '>') (> 0)
  skipSpaces
  states <- parseList (many ((satisfy isLetter) <|> (satisfy isDigit) <|> (char '_')))
    (char ',') (char '<') (char '>') (> 0)
  skipSpaces
  inits <- parseList (keywords_satisfy states (\c -> c == ',' || c == '>' || c == ' '))
   (char ',') (char '<') (char '>') (== 1)
  skipSpaces
  terms <- parseList (keywords_satisfy states (\c -> c == ',' || c == '>' || c == ' '))
   (char ',') (char '<') (char '>') (>= 0)
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

  
-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
-- Pick appropriate types for s and q
parseAutomaton :: String -> Maybe (Automaton String String)
parseAutomaton input = snd <$> (runParser $ parserAutomaton) input

test = parseAutomaton "<a,b,c> <1,2,3> <1> <3> <(1,a,2),(2,b,3),(3,c,1)>"
test1 = parseAutomaton "<aa, bb, cc> <stone, sttwo> <stone> <sttwo> <(stone, ccc, sttwo), (sttwo, bb, stone)>"
test2 = parseAutomaton "<a> <b> <b> <b> <(a, a, b)>"
test3 = parseAutomaton "<aa, bb, cc> <stone, sttwo> <stone> <sttwo> <(stone, cc, sttwo), (sttwo, bb, stone)>"
test4 = parseAutomaton "<aa, bb, cc> <stone, sttwo> <stone> <> <>"
test6 = parseAutomaton "<0, 1> <A, B, C, D, E, F, G> <A> <F, G> <(A, 0, C), (A, 1, B), (B, 0, C), (B, 1, A), (C, 0, D), (C, 1, D), (D, 0, E), (D, 1, F), (E, 0, F), (E, 1, G), (F, 0, F), (F, 1, F), (G, 0, G), (G, 1, F)>"
