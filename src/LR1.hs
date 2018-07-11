{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module LR1 where

import Data.List (union, partition, intersperse)
import Data.Maybe (fromMaybe)

import Grammar


---------------------------------------------------------------------
-- LR(1) table
---------------------------------------------------------------------

type LR1Item = ( Symbol   -- left side of production rule
               , [Symbol] -- prefix of right side on stack
               , [Symbol] -- sufix of right side
               , [String] -- lookahead symbols
               )

type LR1State = [LR1Item]

type LR1Transition = (LR1State,Symbol,LR1State)

type LR1Table = ([LR1State],[LR1Transition])


myGroupBy _ [] = []
myGroupBy eq (x:xs) = (x:ys) : myGroupBy eq zs
  where
    (ys,zs) = partition (eq x) xs


itemsExpectSameSymbol (_,_,symbol1:_,_) (_,_,symbol2:_,_) = symbol1 == symbol2
itemsExpectSameSymbol _ _ = False

advance (left,right1,symbol:right2,lookaheads) = (left,symbol:right1,right2,lookaheads)

makeMove productions nullables firsts state (sts,trs) (items@((_,_,symbol:_,_):_))
  | symbol /= T "$" = (union sts [state'], trs ++ [(state,symbol,state')])
  where
    state' = closure productions nullables firsts (map advance items)
makeMove _ _ _ _ table _ = table

makeMoves productions nullables firsts state =
  foldl (makeMove productions nullables firsts state) ([],[]) (myGroupBy itemsExpectSameSymbol state)

buildLR1 productions nullables firsts oldStates [] transitions = (oldStates, transitions)
buildLR1 productions nullables firsts oldStates (s:ss) transitions
  | elem s oldStates =
      buildLR1 productions nullables firsts oldStates ss transitions
  | otherwise =
      let (newStates, newTransitions) = makeMoves productions nullables firsts s
      in buildLR1 productions nullables firsts (s:oldStates) (ss ++ newStates) (transitions ++ newTransitions)
                                                                                   
mkItem lookaheads (left::=right) = (left,[],right,lookaheads)

newItems productions (nt,lookaheads) =
  map (mkItem lookaheads) (filter ((==nt) . lhs) productions)

mergeItems s1 s2 =
  foldl mergeItem s1 s2
  where
    mergeItem s1 (item2 @ (left,right1,right2,lookaheads)) =
      loop s1 []
      where
        loop [] s3 = reverse (item2:s3)
        loop (item1 @ (l,r1,r2,la) : s1) s3 =
          if l == left && r1 == right1 && r2 == right2
          then reverse ((l,r1,r2,union la lookaheads):s3) ++ s1
          else loop s1 (item1:s3)

closure productions nullables firsts state =
  closure' state state
  where
    closure' [] s2
      | s2 == state = s2
      | otherwise   = closure productions nullables firsts s2
    closure' ((_,_,N nt:right2,lookaheads):rest) s2 =
      closure' rest (mergeItems s2 (newItems productions (N nt, lookaheads')))
      where
        lookaheads' = foldl (\xs x -> union xs (first (right2 ++ [T x]) nullables firsts)) [] lookaheads
    closure' (_:rest) s2 =
      closure' rest s2

lr1 :: Grammar -> LR1Table
lr1 (Grammar productions) =
  buildLR1 productions nullables firsts [] [initialState] []
  where
    nullables = nullableSet (Grammar productions)

    firsts = firstSets (Grammar productions) nullables

    startSymbol = lhs (head productions)

    initialState = closure productions nullables firsts (newItems productions (startSymbol,["?"]))
    


ppLR1Item :: LR1Item -> String
ppLR1Item (left,right1,right2,lookaheads) =
  show left ++ " -> " ++
  unwords (intersperse " " (map show (reverse right1))) ++ " . " ++
  unwords (intersperse " " (map show right2)) ++ " , " ++
  unwords (intersperse " " lookaheads)

ppLR1State transitions (items,number) =
  "STATE " ++ show number ++ "\n" ++
  unlines (map ppLR1Item items) ++
  unlines (map ppTransition (filter ((== number) . fst3) transitions)) ++
  unlines (map ppReduction (filter (null . trd4) items)) ++
  if any ((== [T "$"]) . trd4) items
  then "accept on $\n"
  else ""

ppTransition (state1, symbol, state2) =
  unwords ["(", show state1, ",", show symbol, ")", "--->", show state2]

ppReduction (left,right1,_,lookaheads) =
  unwords (intersperse " " [ "reduction by rule"
                           , show left
                           , "-->"
                           , unwords (map show (reverse right1))
                           , "on"
                           , show lookaheads
                           ])

ppLR1Table (states, transitions) =
  unlines [ "STATES"
          , ""
          , unlines (map (ppLR1State numberedTransitions) numberedStates)
          ]
  
  where
    numberedStates = zip (reverse states) [1..]
    numberedTransitions = map (\(st1,sym,st2) ->
                                 ( fromMaybe 0 (lookup st1 numberedStates)
                                 , sym
                                 , fromMaybe 0 (lookup st2 numberedStates)
                                 ))
                              transitions
