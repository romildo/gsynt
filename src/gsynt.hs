{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import Data.Char (isSpace)

import Data.List (union, groupBy, sort)
import Data.Maybe (fromMaybe)

import Text.Parsec

import System.Console.GetOpt
import System.Environment (getProgName, getArgs)
import Control.Monad (when, (>=>))
import System.IO (IOMode(..), withFile, hGetContents)

data GrammarSymbol
  = T  String
  | NT String
  deriving (Eq, Ord)

instance Show GrammarSymbol where
  show (T  x) = "\"" ++ x ++ "\""
  show (NT x) = x


-- A grammar is given by a list of production rules. Each production
-- rule is given by a non empty list of grammar symbols, whose head is
-- the left hand side of the rule, and whose tail is the right hand side
-- of the rule.
type Grammar = [ [ GrammarSymbol ] ]

showGrammar g = showG g
  where
    showG [] = ""
    showG ((nt:rhs):rs) = width (show nt) len ++ "->" ++
                          foldr (\x s -> " " ++ show x ++ s) "" rhs ++
                          "\n" ++
                          showG rs

    len = maximum (map (length . show . head) g)

    width str n = str ++ " " ++ take (n - length str) (repeat '-')


nullableSet :: Grammar -> [ String ]
nullableSet g = go g [] []
    where
      go [] set1 set2 | set1 == set2 = set2
                      | otherwise    = go g set2 set2

      go ((NT x:xs):ps) set1 set2
          | isNullable xs set2 = go ps set1 (insert x set2)
          | otherwise          = go ps set1 set2

isNullable :: [ GrammarSymbol ] -> [ String ] -> Bool
isNullable [] set = True
isNullable (T _:_) set = False
isNullable (NT x:xs) set = elem x set && isNullable xs set


firstSets :: Grammar -> [String] -> [ (String,[String]) ]
firstSets g nullables = go g [] []
    where
      go [] f1 f2 | f1 == f2  = f2
                  | otherwise = go g f2 f2

      go ((NT x:xs):ps) f1 f2 = go ps f1 (merge (x,first xs nullables f2) f2)

first [] _ _  = []
first (T x:xs) _ _ = [x]
first (NT x:xs) nullables firsts
    | elem x nullables = union first_x (first xs nullables firsts)
    | otherwise        = first_x
    where
      first_x = look x firsts
                                    

followSets grammar nullables firsts = go grammar [] []
    where
      go [] fol1 fol2 | fol1 == fol2 = fol2
                      | otherwise    = go grammar fol2 fol2
      go ((NT x:xs):ps) fol1 fol2 = go ps fol1 (inLhs (reverse xs) (inRhs xs fol2))
          where
            inRhs [] fol = fol
            inRhs (NT y:ys) fol = inRhs ys (merge (y,first ys nullables firsts) fol)
            inRhs (T _:ys) fol = inRhs ys fol

            inLhs [] fol = fol
            inLhs (T _:_) fol = fol
            inLhs (NT y:ys) fol | elem y nullables = inLhs ys fol1
                                | otherwise        = fol1
                where
                  fol1 = merge (y, look x fol) fol



ll1Table grammar = go grammar []
    where
      go [] t = organizeTable t
      go ((NT x:xs):ps) t | isNullable xs nullables = go ps t2
                          | otherwise               = go ps t1
          where
            addActions ys table = foldr insert table (map (\y -> ((x,y),xs)) ys)
            t1 = addActions (first xs nullables firsts) t
            t2 = addActions (look x follows) t1

      nullables = nullableSet grammar
      firsts = firstSets grammar nullables
      follows = followSets grammar nullables firsts


organizeTable table = map sepNT (groupBy itemEq (sort table))
    where
      itemEq ((x1,_),_) ((x2,_),_) = x1 == x2

      sepNT tab@(((nt,_),_):_) = (nt, map (\((_,t),rhs) -> (t,rhs)) tab)


showTable table = foldr f "" table
    where
      f (nt,rules) s = "\n\n<" ++ nt ++ "> ::=" ++ foldr g s rules
      g (t,rhs) s = "\n\t" ++ t ++ "\t==>\t" ++ foldr h s rhs
      h x s = show x ++ " " ++ s

---------------------------------------------------------------------
-- LR(1) table
---------------------------------------------------------------------
{-
lr1 g = ()
  where
    start = head (head g)

    states = buildStates [(start, "?")]

    buildStates [] = []
    buildStates ((nt,lookahead):nts) = map (mkItem lookahead) (filter ((==nt) . head) g)

    mkItem lookahead (left:right) = closure [(left,[],right,lookahead)]
    
    closure state = closure' state state
      where
        closure' [] s2 | state == s2 = s2
                       | otherwise = closure' s2 s2
        closure' ((left,right1,NT nt:right2,lookahead):rest) s2 = closure' rest (buildStates [(NT nt, first (right2 ++ [lookahead]))])
-}


---------------------------------------------------------------------
-- Auxiliary definitions
---------------------------------------------------------------------

insert x [] = [ x ]
insert x set | elem x set = set
             | otherwise  = x : set

merge pair [] = [pair]
merge (key,val) ((k,v):rest) | key == k  = (key, union val v) : rest
                             | otherwise = (k,v) : merge (key,val) rest


look key assoc = fromMaybe [] (lookup key assoc)


showAssocList [] = ""
showAssocList ((x,ys):rest) = "\n" ++ show x ++ "\t:\t" ++
                              foldr (\t s -> show t ++ " " ++ s) "" ys ++
                              showAssocList rest


testGrammar g = do putStrLn ">>>>>>>>>> Nullable set: "
                   putStrLn (show nullables)
                   putStrLn ""
                   putStrLn ">>>>>>>>>> First sets: "
                   putStrLn (showAssocList firsts)
                   putStrLn ""
                   putStrLn ">>>>>>>>>> Follow sets: "
                   putStrLn (showAssocList follows)
                   putStrLn ""
                   putStrLn ">>>>>>>>>> LL(1) table: "
                   putStrLn (showTable (ll1Table g))
    where
      nullables = nullableSet g
      firsts = firstSets g nullables
      follows = followSets g nullables firsts


-- -----------------------------
-- LR(0) table

data Item = Item GrammarSymbol [GrammarSymbol] [GrammarSymbol]
            deriving (Eq)

instance Show Item where
  show (Item nt xs ys) = show nt ++ " ->" ++ foldr f "" xs ++ " ."  ++ foldr f "" ys
    where
      f x s = " " ++ show x ++ s

showState items = unlines (map show items)                         

start grm@((nt:rhs):_) = closure grm [Item nt [] rhs]

closure grm items
  | items == items' = items
  | otherwise       = closure grm items'
  where
    items' = foldr closure1 items items
    closure1 (Item _ _ (nt@(NT _):_)) items = foldr (checkRule nt) items grm
    closure1 _                        items = items
    checkRule nt (nt':rhs) items | nt == nt' = insert (Item nt [] rhs) items
                                 | otherwise = items

lr0 grm = start grm

showLR0 = showState


-- -----------------------------
-- Grammar parser


grammarParser = do spaces
                   rules <- many rule
                   eof
                   return rules

rule = do nt <- lexeme nonterminal
          lexeme (string "->")
          right <- many (lexeme (terminal <|> nonterminal))
          spaces
          return (nt : right)

nonterminal = do symbol <- many1 (letter <|>
                                  digit <|>
                                  char '_' <|>
                                  char '\'')
                 return (NT symbol)

terminal = do char '"'
              symbol <- many1 (satisfy (\x -> x /= '"' && x /= '\\') <|>
                               (char '\\' >> ((char '"' >> return '"') <|>
                                              (char '\\' >> return '\\'))))
              char '"'
              return (T symbol)

lexeme p = do x <- p
              skipMany (satisfy (\x -> isSpace x && x /= '\n'))
              return x

-- -----------------------------






data Flag = Nullable | First | Follow | LL1 | LR0
          deriving (Eq,Show)

options =
    [ Option ['n'] ["nullables"] (NoArg Nullable) "print the nullables table"
    , Option ['f'] ["first"    ] (NoArg First)    "print the first table"
    , Option ['w'] ["follow"   ] (NoArg Follow)   "print the follow table"
    , Option ['l'] ["ll1"      ] (NoArg LL1)      "print the LL(1) parse table"
    , Option ['0'] ["lr0"      ] (NoArg LR0)      "print the LR(0) parse table"
    ]

usedOpts prog args =
    case getOpt Permute options args of
      (o,[f],[]) -> return (o,Just f)
      (o,[ ],[]) -> return (o,Nothing)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where
      header = "Usage: " ++ prog ++ " [OPTION...] [FILE]"

process o f input =
    case (parse grammarParser f input) of
      Left err -> print err
      Right g  -> do putStrLn ">>>>>>>>>> Grammar: "
                     putStrLn (showGrammar g)
                     putStrLn ""
                     let nullables = nullableSet g
                         firsts = firstSets g nullables
                         follows = followSets g nullables firsts
                     when (elem Nullable o) $
                       do putStrLn ">>>>>>>>>> Nullable set: "
                          putStrLn (show nullables)
                          putStrLn ""
                     when (elem First o) $
                       do putStrLn ">>>>>>>>>> First sets: "
                          putStrLn (showAssocList firsts)
                          putStrLn ""
                     when (elem Follow o) $
                       do putStrLn ">>>>>>>>>> Follow sets: "
                          putStrLn (showAssocList follows)
                          putStrLn ""
                     when (elem LL1 o) $
                       do putStrLn ">>>>>>>>>> LL(1) table: "
                          putStrLn (showTable (ll1Table g))
                          putStrLn ""
                     when (elem LR0 o) $
                       do putStrLn ">>>>>>>>>> LR(0) table: "
                          putStrLn (showLR0 (lr0 g))
                          putStrLn ""

main =
  do putStrLn "LL(1) Parser Generator"
     putStrLn "Version 0.1"
     putStrLn ""
     args <- getArgs
     prog <- getProgName
     (o,mf) <- usedOpts prog args
     case mf of
       Just f -> withFile f ReadMode (hGetContents >=> process o f)
       Nothing -> getContents >>= process o "stdin"
