{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns      #-}

module Automaton where

import           Data.Char
import           Data.List
import qualified Data.Map                 as Map
import           Data.Maybe
import qualified Data.MultiMap            as MultiMap
import qualified Data.Set                 as Set

import           Combinators
import           Control.Monad.State.Lazy

type Set = Set.Set

type Map = Map.Map

type MultiMap = MultiMap.MultiMap

data Automaton s q = Automaton
  { sigma     :: Set s
  , states    :: Set q
  , initState :: q
  , termState :: Set q
  , epsilon   :: s
  , devil     :: q
  , delta     :: MultiMap (q, s) (Maybe q)
  } deriving (Show, Eq)

instance (Show k, Show v) => Show (MultiMap k v) where
  show = show . MultiMap.toList

instance (Ord k, Ord v) => Eq (MultiMap k v) where
  x == y = (sort $ MultiMap.toList x) == (sort $ MultiMap.toList y)

-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: Eq a => Automaton a b -> Bool
isDFA a =
  MultiMap.numKeys (delta a) == MultiMap.numValues (delta a) &&
  notElem (epsilon a) (Prelude.fmap snd $ MultiMap.keys (delta a))

-- Checks if the automaton is nondeterministic (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Eq a => Automaton a b -> Bool
isNFA _ = True

-- Checks if the automaton is complete (there exists a transition for each state and each input symbol)
isComplete :: Eq a => Automaton a b -> Bool
isComplete a =
  (isDFA a) &&
  MultiMap.numKeys (delta a) ==
  fromIntegral ((Set.size (sigma a)) * (Set.size (states a)))

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: Automaton String String -> Bool
isMinimal a = (minimize a) == removeUnreachable (if not $ isDFA a
   then error "Non determenistic"
   else if isComplete a
          then a
          else transformToCompleteDeterministic a)

transformToCompleteDeterministic ::
     (Ord a, Ord b) => Automaton a b -> Automaton a b
transformToCompleteDeterministic a =
  if isDFA a
    then Automaton
           (sigma a)
           (states a)
           (initState a)
           (termState a)
           (epsilon a)
           (devil a)
           (foldr
              (\d ds ->
                 if MultiMap.lookup d ds == []
                   then MultiMap.insert d (Just $ devil a) ds
                   else ds)
              (delta a)
              [ (state, sig)
              | state <- Set.elems $ states a
              , sig <- Set.elems $ sigma a
              ])
    else error "Not Deterministic Automaton"

data DeterminizeState s q = DeterminizeState
  { queue        :: Set q
  , newStates    :: Set q
  , newTerminals :: Set q
  , newDeltas    :: MultiMap (q, s) (Maybe q)
  }

determinize :: Automaton String String -> Automaton String String
determinize a =
  if elem (epsilon a) (Prelude.fmap snd $ MultiMap.keys (delta a))
    then error "Found an epsilon-transition"
    else if isDFA a
           then a
           else determinize' a
  where
    determinize' a =
      evalState (determinize'' a) $
      DeterminizeState
        (Set.fromList [[initState a]])
        Set.empty
        Set.empty
        MultiMap.empty
      where
        determinize'' ::
             Automaton String String
          -> State (DeterminizeState String [String]) (Automaton String String)
        determinize'' a = do
          queue' <- gets queue
          if null queue'
            then do
              sts <- gets newStates
              trms <- gets newTerminals
              dlts <- gets newDeltas
              return $
                Automaton
                  (sigma a)
                  (Set.map (intercalate ",") sts)
                  (initState a)
                  (Set.map (intercalate ",") trms)
                  (epsilon a)
                  (devil a)
                  (MultiMap.map
                     (\x ->
                        case x of
                          Just v -> Just (intercalate "," v)
                          _      -> Just $ devil a) $
                   (MultiMap.mapKeys
                      (\(xs, s) -> ((intercalate "," xs), s))
                      dlts))
            else do
              let sts = sort $ Set.elemAt 0 queue'
              modify
                (\s ->
                   s
                     { queue = Set.deleteAt 0 (queue s)
                     , newStates = Set.insert sts (newStates s)
                     })
              when (any (\v -> Set.member v (termState a)) sts) $
                modify
                  (\s -> s {newTerminals = Set.insert sts (newTerminals s)})
              let outst =
                    sort $
                    nub $
                    [ end
                    | ((begin, _), Just end) <- MultiMap.toList (delta a)
                    , elem begin sts
                    ]
              let outc =
                    nub $
                    [ c
                    | ((begin, c), _) <- MultiMap.toList (delta a)
                    , elem begin sts
                    ]
              newsts <- gets newStates
              when (Set.notMember outst newsts && outst /= []) $
                modify (\s -> s {queue = Set.insert outst (queue s)})
              modify
                (\s ->
                   s
                     { newDeltas =
                         foldr
                           (\c m -> MultiMap.insert (sts, c) (Just outst) m)
                           (newDeltas s)
                           outc
                     })
              determinize'' a

epsilonClosure :: Automaton String String -> Automaton String String
epsilonClosure a =
  if notElem (epsilon a) (Prelude.fmap snd $ MultiMap.keys (delta a))
    then a
    else let epsilons =
               [ (begin, end)
               | ((begin, c), Just end) <- MultiMap.toList (delta a)
               , c == epsilon a
               ]
          in Automaton
               (sigma a)
               (foldr
                  (\(_, end) s ->
                     if notElem
                          end
                          [ e
                          | ((_, c), Just e) <- MultiMap.toList (delta a)
                          , c /= epsilon a
                          ]
                       then Set.delete end s
                       else s)
                  (states a)
                  epsilons)
               (initState a)
               (foldr
                  (\(begin, end) s ->
                     if Set.member end (termState a)
                       then if notElem
                                 end
                                 [ e
                                 | ((_, c), Just e) <- MultiMap.toList (delta a)
                                 , c /= epsilon a
                                 ]
                              then Set.delete end $ Set.insert begin s
                              else Set.insert begin s
                       else s)
                  (termState a)
                  epsilons)
               (epsilon a)
               (devil a)
               (foldr
                  (\(begin, end) d ->
                     foldr
                       (\k m -> MultiMap.delete k m)
                       (foldr
                          (\(k, v) m -> MultiMap.insert k v m)
                          d
                          [ ((begin, c), Just e)
                          | ((b, c), Just e) <- MultiMap.toList (delta a)
                          , b == end
                          ])
                       [ (b, c)
                       | ((b, c), Just e) <- MultiMap.toList (delta a)
                       , c == epsilon a ||
                           notElem
                             b
                             [ e
                             | ((_, c), Just e) <- MultiMap.toList (delta a)
                             , c /= epsilon a
                             ]
                       ])
                  (delta a)
                  epsilons)

dfs :: Eq q => q -> MultiMap (q, s) (Maybe q) -> [q]
dfs s m = s : dfs' s []
  where
    dfs' s sts =
      nub $
      foldr
        (\s' sts' -> dfs' s' (s' : sts'))
        sts
        [ e
        | ((b, _), Just e) <- MultiMap.toList m
        , b == s && notElem e sts && e /= s
        ]

removeUnreachable :: Automaton String String -> Automaton String String
removeUnreachable a =
  let reachable = dfs (initState a) (delta a)
   in Automaton
        (sigma a)
        (foldr
           (\st sts ->
              if elem st reachable
                then sts
                else Set.delete st sts)
           (states a)
           (Set.toList $ states a))
        (initState a)
        (foldr
           (\st sts ->
              if elem st reachable
                then sts
                else Set.delete st sts)
           (termState a)
           (Set.toList $ termState a))
        (epsilon a)
        (devil a)
        (foldr
           (\k m -> MultiMap.delete k m)
           (delta a)
           [ (b, c)
           | ((b, c), Just e) <- MultiMap.toList (delta a)
           , notElem b reachable || notElem e reachable
           ])

data MinimizeState q = MinimizeState
  { queue' :: Set (q, q)
  , table  :: Set (q, q)
  }

minimize :: Automaton String String -> Automaton String String
minimize a =
  minimize' $ 
  removeUnreachable
    (if not $ isDFA a
       then error "Non determenistic"
       else if isComplete a
              then a
              else transformToCompleteDeterministic a)
  where
    reverseMapping a' =
      MultiMap.fromList
        [ ((end, sigma), begin)
        | ((begin, sigma), Just end) <- MultiMap.toList (delta a')
        ]
    minimize' a =
      evalState (minimize'' a) $
      MinimizeState
        (Set.fromList
           [ (x, y)
           | x <- (Set.toList $ states a)
           , y <- (Set.toList $ termState a)
           , notElem x (termState a)
           ])
        (Set.fromList
           [ (x, y)
           | x <- (Set.toList $ states a)
           , y <- (Set.toList $ termState a)
           , notElem x (termState a)
           ])
      where
        minimize'' ::
             Automaton String String
          -> State (MinimizeState String) (Automaton String String)
        minimize'' a = do
          queue'' <- gets queue'
          if null queue''
            then do
              table' <- gets table
              let sts =
                    [ (s, ss)
                    | s <- Set.toList $ states a
                    , ss <- [sort [y | (x, y) <- Set.toList table', x == s]]
                    ]
              let groups = groupBy (\st1 st2 -> snd st1 == snd st2) sts
              let result = map (\ss -> map fst ss) groups
              return $
                Automaton
                  (sigma a)
                  (Set.fromList $ map (intercalate ",") result)
                  (intercalate "," $
                   head [x | x <- result, elem (initState a) x])
                  (Set.fromList $
                   map
                     (intercalate ",")
                     [x | x <- result, any (\x' -> elem x' (termState a)) x])
                  (epsilon a)
                  (devil a)
                  (MultiMap.fromList
                     [ (((intercalate "," x), y), Just (intercalate "," z))
                     | x <- result
                     , z <- result
                     , y <- Set.toList $ sigma a
                     , any
                         (\x' ->
                            any
                              (\x'' ->
                                 case x'' of
                                   Just x''' -> elem x''' z
                                   Nothing   -> False) $
                            MultiMap.lookup (x', y) (delta a))
                         x
                     ])
            else do
              let (x, y) = Set.elemAt 0 queue''
              table' <- gets table
              let newPairs =
                    [ (if f < s then (f, s) else (s, f))
                    | (x', y') <-
                        [((x, s'), (y, s')) | s' <- Set.toList $ sigma a]
                    , f <- MultiMap.lookup x' $ reverseMapping a
                    , s <- MultiMap.lookup y' $ reverseMapping a
                    , f /= s
                    , not $ Set.member (if f < s then (f, s) else (s, f)) table'
                    ]
              modify
                (\s ->
                   s
                     { queue' =
                         Set.union
                           (Set.deleteAt 0 (queue' s))
                           (Set.fromList newPairs)
                     })
              modify (\s -> s {table = Set.union (table s) (Set.fromList newPairs)})
              minimize'' a

parserAutomaton :: Parser Char String (Automaton [Char] [Char])
parserAutomaton = do
  let skipSpaces = many $ satisfy isSpace
  alphabet <-
    parseList
      (many
         ((satisfy isLetter) <|> (satisfy isDigit) <|> (char '_') <|>
          (char '\\')))
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
            (many
               ((satisfy isLetter) <|> (satisfy isDigit) <|> (char '_') <|>
                (char '\\')))
            (char ',')
            (char '(')
            (char ')')
            (== 3)
        if elem begin states
          then if elem end ("\\devil" : states)
                 then if elem sigma ("\\epsilon" : alphabet)
                        then return ((begin, sigma), Just end)
                        else Combinators.fail $
                             "Sigma '" ++ sigma ++ "' is not in the alphabet"
                 else Combinators.fail "End state is not in the states"
          else Combinators.fail "Begin state is not in the states"
  delts <- parseList triplets (char ',') (char '<') (char '>') (>= 0)
  skipSpaces
  stringEof
  return $
    Automaton
      (Set.fromList alphabet)
      (Set.fromList $ "\\devil" : states)
      (head inits)
      (Set.fromList terms)
      ("\\epsilon")
      ("\\devil")
      (MultiMap.fromList delts)

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
-- Pick appropriate types for s and q
parseAutomaton :: String -> Either [String] (Automaton String String)
parseAutomaton input =
  case runParser parserAutomaton (stream input) of
    Left e       -> Left e
    Right (_, a) -> Right a

fromEither :: Either a b -> b
fromEither (Left _)  = undefined
fromEither (Right b) = b

test = parseAutomaton "<a,b,c> <1,2,3> <1> <3> <(1,a,2),(2,b,3),(3,c,1)>"

test1 =
  parseAutomaton
    "<aa, bb, cc> <stone, sttwo> <stone> <sttwo> <(stone, ccc, sttwo), (sttwo, bb, stone)>"

test2 = parseAutomaton "<a> <b> <b> <b> <(a, a, b)>"

test3 =
  parseAutomaton
    "<aa, bb, cc> <stone, sttwo> <stone> <sttwo> <(stone, cc, sttwo), (sttwo, bb, stone)>"

test4 =
  parseAutomaton
    "<aa> <stone, sttwo> <stone> <sttwo> <(stone, \\epsilon, sttwo)>"

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
    "<aa> <stone, sttwo> <stone> <sttwo> <(stone, \\epsilon, sttwo)>"

test_IS_COMPLETE_FALSE =
  isComplete $
  fromEither $ parseAutomaton "<a> <1> <1> <1> <(1, a, 1), (1, a, 1)>"

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

testTransformToCompleteDeterministic =
  isComplete $ transformToCompleteDeterministic $ fromEither test3

testNFAtoDFA =
  parseAutomaton
    "<a,b,c> <1,2,3> <1> <3> <(1,c,2),(1,c,3),   (2,a,3),(2,b,3),(2,c,3),    (3,a,1),(3,b,1),(3,c,1)>"

testEpsilonClosure1 =
  parseAutomaton "<a,b,c> <1,2,3> <1> <3> <(1,\\epsilon,2),(2,a,3)>"

testEpsilonClosure2 =
  parseAutomaton "<a,b,c> <1,2,3,4> <1> <3> <(1,\\epsilon,2),(2,a,3),(4,b,2)>"

testEpsilonClosure3 = parseAutomaton "<a,b,c> <1,2> <1> <2> <(1,\\epsilon,2)>"

testMinimization =
  minimize $
  fromEither $
  parseAutomaton
    "<0,1> <A,B,C,D,E,F,G> <A> <F,G> <(A,0,C),(A,1,B),(B,1,A),(B,0,C),(C,0,D),(C,1,D),(D,0,E),(D,1,F),(E,0,F),(E,1,G),(F,0,F),(F,1,F),(G,0,G),(G,1,F)>"

testIsMinimalTrue = isMinimal $ fromEither $ test3

testIsMinimalTrue2 = isMinimal $ fromEither $ parseAutomaton "<a,b,c,d> <0,1,2,3,4,5> <0> <3,4,5> <(0, a, 1), (0, b, 2), (0, d, 2), (0, c, 3), (1, a, 4), (2, b, 4), (2, a, 5), (3, a, 4), (5, a, 5)>"

testIsMinimalFalse =
  isMinimal $
  fromEither $
  parseAutomaton
    "<0,1> <A,B,C,D,E,F,G> <A> <F,G> <(A,0,C),(A,1,B),(B,1,A),(B,0,C),(C,0,D),(C,1,D),(D,0,E),(D,1,F),(E,0,F),(E,1,G),(F,0,F),(F,1,F),(G,0,G),(G,1,F)>"