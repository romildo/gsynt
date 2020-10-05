{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module LR1 where

import Data.List (union, partition, intersperse)
import Data.Maybe (fromMaybe)

import Grammar

import Text.LaTeX (Texy(texy), LaTeXT_, (&), noindent, textbf, document, maketitle, title, raw, author, usepackage, article, documentclass, (<>), lnbk, newline, vspace, tabular, Measure(..), TableSpec(..))
import Text.LaTeX.Packages.Inputenc (inputenc, utf8)


---------------------------------------------------------------------
-- LR(1) table
---------------------------------------------------------------------

-- type LR1Item = ( Symbol   -- left side of production rule
--                , [Symbol] -- prefix of right side on stack
--                , [Symbol] -- sufix of right side
--                , [String] -- lookahead symbols
--                )

data LR1Item = LR1Item
               { left      :: Symbol   -- left side of production rule
               , right1    :: [Symbol] -- prefix of right side on stack
               , right2    :: [Symbol] -- sufix of right side
               , lookahead :: [String] -- lookahead symbols
               }
  deriving (Eq, Show)

type LR1State = [LR1Item]

data LR1Transition a = LR1Transition
                       { from   :: a
                       , symbol :: Symbol
                       , to     :: a
                       }
  deriving (Show)

data LR1Table = LR1Table
                { states      :: [LR1State]
                , transitions :: [LR1Transition LR1State]
                }
  deriving (Show)

myGroupBy _ [] = []
myGroupBy eq (x:xs) = (x:ys) : myGroupBy eq zs
  where
    (ys,zs) = partition (eq x) xs


itemsExpectSameSymbol (LR1Item{right2=symbol1:_}) (LR1Item{right2=symbol2:_}) = symbol1 == symbol2
itemsExpectSameSymbol _ _ = False

advance item@(LR1Item{right1, right2=symbol:right2}) =
  item{right1=symbol:right1, right2}

makeMove productions nullables firsts state (sts,trs) (items@((LR1Item _ _ (symbol:_) _):_))
  | symbol /= T "$" = (union sts [state'], trs ++ [LR1Transition state symbol state'])
  where
    state' = closure productions nullables firsts (map advance items)
makeMove _ _ _ _ table _ = table

makeMoves productions nullables firsts state =
  foldl (makeMove productions nullables firsts state) ([],[]) (myGroupBy itemsExpectSameSymbol state)

buildLR1 productions nullables firsts oldStates [] transitions = LR1Table oldStates transitions
buildLR1 productions nullables firsts oldStates (s:ss) transitions
  | elem s oldStates =
      buildLR1 productions nullables firsts oldStates ss transitions
  | otherwise =
      let (newStates, newTransitions) = makeMoves productions nullables firsts s
      in buildLR1 productions nullables firsts (s:oldStates) (ss ++ newStates) (transitions ++ newTransitions)
                                                                                   
mkItem lookaheads (left::=right) = LR1Item left [] right lookaheads

newItems productions (nt,lookaheads) =
  map (mkItem lookaheads) (filter ((==nt) . lhs) productions)

mergeItems s1 s2 =
  foldl mergeItem s1 s2
  where
    mergeItem s1 (item2 @ (LR1Item left right1 right2 lookaheads)) =
      loop s1 []
      where
        loop [] s3 = reverse (item2:s3)
        loop (item1 @ (LR1Item l r1 r2 la) : s1) s3 =
          if l == left && r1 == right1 && r2 == right2
          then reverse ((LR1Item l r1 r2 (union la lookaheads)):s3) ++ s1
          else loop s1 (item1:s3)

closure productions nullables firsts state =
  closure' state state
  where
    closure' [] s2
      | s2 == state = s2
      | otherwise   = closure productions nullables firsts s2
    closure' ((LR1Item _ _ (N nt:right2) lookaheads):rest) s2 =
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
ppLR1Item (LR1Item left right1 right2 lookaheads) =
  show left ++ " -> " ++
  unwords (intersperse " " (map show (reverse right1))) ++ " . " ++
  unwords (intersperse " " (map show right2)) ++ " , " ++
  unwords (intersperse " " lookaheads)

ppLR1State transitions (items,number) =
  "STATE " ++ show number ++ "\n" ++
  unlines (map ppLR1Item items) ++
  unlines (map ppTransition (filter ((== number) . from) transitions)) ++
  unlines (map ppReduction (filter (null . right2) items)) ++
  if any ((== [T "$"]) . right2) items
  then "accept on $\n"
  else ""

ppTransition (LR1Transition state1 symbol state2) =
  unwords ["(", show state1, ",", show symbol, ")", "--->", show state2]

ppReduction (LR1Item left right1 _ lookaheads) =
  unwords (intersperse " " [ "reduction by rule"
                           , show left
                           , "-->"
                           , unwords (map show (reverse right1))
                           , "on"
                           , show lookaheads
                           ])

ppLR1Table (LR1Table states transitions) =
  unlines [ "STATES"
          , ""
          , unlines (map (ppLR1State numberedTransitions) numberedStates)
          ]
  where
    (numberedStates, numberedTransitions) =
      numberStates (states, transitions)

numberStates :: ([LR1State], [LR1Transition LR1State]) -> ([(LR1State, Integer)], [LR1Transition Integer])
numberStates (states, transitions) =
  (numberedStates, numberedTransitions)
  where
    numberedStates = zip (reverse states) [1..]
    numberedTransitions = map (\(LR1Transition st1 sym st2) ->
                                 (LR1Transition
                                  (fromMaybe 0 (lookup st1 numberedStates))
                                  sym
                                  (fromMaybe 0 (lookup st2 numberedStates))
                                 ))
                              transitions

-- ---------------------------------------------------------------------------

instance Texy LR1Item where
  texy (LR1Item left right1 right2 lookaheads) =
    texy left &
    mconcat (intersperse mempty (map texy (reverse right1))) &
    mconcat (intersperse mempty (map texy right2)) &
    mconcat (intersperse mempty (map (texy . T) lookaheads))

--instance Texy LR1State where
texyLR1State items =
    tabular
      Nothing
      [ Separator mempty
      , LeftColumn
      , Separator (raw "$\\rightarrow$")
      , LeftColumn
      , Separator "."
      , LeftColumn
      , Separator ","
      , LeftColumn
      , Separator mempty
      ]
      (mconcat (intersperse lnbk (map texy items)))

instance Texy a => Texy (LR1Transition a) where
  texy (LR1Transition state1 symbol state2) =
    raw "(" <> texy state1 <> raw "," <> texy symbol <> raw ")" <>
    raw "$\\mapsto$" <>
    texy state2

instance Texy LR1Table where
  texy (LR1Table states transitions) =
    mconcat (intersperse newline (map (texyState numberedTransitions) numberedStates))
    where
      (numberedStates, numberedTransitions) =
        numberStates (states, transitions)
      texyState transitions (items, number) =
        noindent <>
        textbf "state " <> texy number <>
        newline <> vspace (Em 0.3) <>
        texyLR1State items <>
        newline <> vspace (Em 0.3) <>
        mconcat (intersperse newline (map texy (filter ((== number) . from) transitions))) <>
        newline <>
        mconcat (intersperse newline (map texyReduction (filter (null . right2) items))) <>
        newline <> vspace (Em 0.3) <>
        (if any ((== [T "$"]) . right2) items
         then "accept on $" <> newline
         else "") <>
        vspace (Em 0.3)
      texyReduction (LR1Item left right1 _ lookaheads) =
        "reduction by rule " <>
        texy (left ::= reverse right1) <>
        " on " <>
        mempty
        --texy lookaheads


latexLR1Table :: Monad m => Grammar -> LaTeXT_ m
latexLR1Table g = do
  documentclass [] article
  usepackage [utf8] inputenc
  raw "\\usepackage[brazilian]{babel}%\n"
  raw "\\usepackage{array}%\n"
  author "José Romildo Malaquias"
  title "LR(1) parsing table"
  document $ do
    maketitle
    texy (lr1 g)
