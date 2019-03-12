module Automaton where

import           Data.Char
import           Data.Maybe
import           Data.MultiMap (MultiMap, fromList, keys, numKeys, numValues,
                                toList)
import qualified Data.Set      as Set

import           Combinators

type Set = Set.Set

data Automaton s q = Automaton
  { sigma     :: Set s
  , states    :: Set q
  , initState :: q
  , termState :: Set q
  , epsilon   :: s
  , delta     :: MultiMap (q, s) (Maybe q)
  } deriving (Show)

instance (Show k, Show v) => Show (MultiMap k v) where
  show = show . toList

-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: Eq a => Automaton a b -> Bool
isDFA a =
  numKeys (delta a) == numValues (delta a) &&
  notElem (epsilon a) (Prelude.fmap snd $ keys (delta a))

-- Checks if the automaton is nondeterministic (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Eq a => Automaton a b -> Bool
isNFA a = True

-- Checks if the automaton is complete (there exists a transition for each state and each input symbol)
isComplete :: Automaton a b -> Bool
isComplete a =
  numKeys (delta a) ==
  fromIntegral ((Set.size (sigma a)) * (Set.size (states a)))

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: Automaton a b -> Bool
isMinimal = undefined

parserAutomaton = do
  let skipSpaces = many $ satisfy isSpace
  alphabet <-
    parseList
      (many ((satisfy isLetter) <|> (satisfy isDigit) <|> (char '_')))
      (char ',')
      (char '<')
      (char '>')
      (> 0)
  skipSpaces
  states <-
    parseList
      (many ((satisfy isLetter) <|> (satisfy isDigit) <|> (char '_')))
      (char ',')
      (char '<')
      (char '>')
      (> 0)
  skipSpaces
  inits <-
    parseList
      (keywords_satisfy states (\c -> c == ',' || c == '>' || c == ' '))
      (char ',')
      (char '<')
      (char '>')
      (== 1)
  skipSpaces
  terms <-
    parseList
      (keywords_satisfy states (\c -> c == ',' || c == '>' || c == ' '))
      (char ',')
      (char '<')
      (char '>')
      (>= 0)
  skipSpaces
  let triplets = do
        [begin, sigma, end] <-
          parseList
            (many ((satisfy isLetter) <|> (satisfy isDigit) <|> (char '_')))
            (char ',')
            (char '(')
            (char ')')
            (== 3)
        if elem begin states
          then if elem end states
                 then if elem sigma alphabet
                        then return ((begin, sigma), Just end)
                        else Combinators.fail "Sigma is not in the alphabet\n"
                 else Combinators.fail "End state is not in the states\n"
          else Combinators.fail "Begin state is not in the states\n"
  delts <- parseList triplets (char ',') (char '<') (char '>') (>= 0)
  return $
    Automaton
      (Set.fromList alphabet)
      (Set.fromList states)
      (head inits)
      (Set.fromList terms)
      ("EPS")
      (fromList delts)

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
-- Pick appropriate types for s and q
parseAutomaton :: String -> Either String (Automaton String String)
parseAutomaton input =
  case runParser parserAutomaton (stream input) of
    Left e       -> Left e
    Right (_, a) -> Right a

fromEither :: Either a b -> b
fromEither (Left _) = undefined
fromEither (Right b) = b

test = parseAutomaton "<a,b,c> <1,2,3> <1> <3> <(1,a,2),(2,b,3),(3,c,1)>"

test1 =
  parseAutomaton
    "<aa, bb, cc> <stone, sttwo> <stone> <sttwo> <(stone, ccc, sttwo), (sttwo, bb, stone)>"

test2 = parseAutomaton "<a> <b> <b> <b> <(a, a, b)>"

test3 =
  parseAutomaton
    "<aa, bb, cc> <stone, sttwo> <stone> <sttwo> <(stone, cc, sttwo), (sttwo, bb, stone)>"

test4 = parseAutomaton "<aa, bb, cc> <stone, sttwo> <stone> <> <>"

test6 =
  parseAutomaton
    "<0, 1> <A, B, C, D, E, F, G> <A> <F, G> <(A, 0, C), (A, 1, B), (B, 0, C), (B, 1, A), (C, 0, D), (C, 1, D), (D, 0, E), (D, 1, F), (E, 0, F), (E, 1, G), (F, 0, F), (F, 1, F), (G, 0, G), (G, 1, F)>"

test_DFA_TRUE =
  isDFA $
  fromEither $
  parseAutomaton
    "<0, 1> <A, B, C, D, E, F, G> <A> <F, G> <(A, 0, C), (A, 1, B), (B, 0, C), (B, 1, A), (C, 0, D), (C, 1, D), (D, 0, E), (D, 1, F), (E, 0, F), (E, 1, G), (F, 0, F), (F, 1, F), (G, 0, G), (G, 1, F)>"

test_DFA_FALSE_MULTI =
  isDFA $
  fromEither $
  parseAutomaton
    "<0, 1> <A, B, C, D, E, F, G> <A> <F, G> <(A, 0, C), (A, 1, B), (B, 0, C), (B, 1, A), (C, 0, D), (C, 1, D), (C, 0, E), (D, 1, F), (E, 0, F), (E, 1, G), (F, 0, F), (F, 1, F), (G, 0, G), (G, 1, F)>"

test_DFA_FALSE_EPS =
  isDFA $
  fromEither $
  parseAutomaton
    "<0, 1, EPS> <A, B, C, D, E, F, G> <A> <F, G> <(A, 0, C), (A, 1, B), (B, 0, C), (B, 1, A), (C, 0, D), (C, 1, D), (D, EPS, E), (D, 1, F), (E, 0, F), (E, 1, G), (F, 0, F), (F, 1, F), (G, 0, G), (G, 1, F)>"

test_IS_COMPLETE_FALSE =
  isComplete $
  fromEither $ parseAutomaton "<a,b,c> <1,2,3> <1> <3> <(1,a,2),(2,b,3),(3,c,1)>"

test_IS_COMPLETE_TRUE =
  isComplete $
  fromEither
    $parseAutomaton
    "<a,b,c> <1,2,3> <1> <3> <(1,a,2),(1,b,2),(1,c,2),   (2,a,3),(2,b,3),(2,c,3),    (3,a,1),(3,b,1),(3,c,1)>"

test_IS_COMPLETE_TRUE_MULTI =
  isComplete $
  fromEither
    $parseAutomaton
    "<a,b,c> <1,2,3> <1> <3> <(1,a,2),(1,b,2),(1,c,2),(1,c,3),   (2,a,3),(2,b,3),(2,c,3),    (3,a,1),(3,b,1),(3,c,1)>"
