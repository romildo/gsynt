{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Grammar where

import Data.List (union, groupBy, sort, mapAccumL, intersperse, find)
import Data.Maybe (fromMaybe)

import Text.LaTeX
import Text.LaTeX.Base.Class (LaTeXC)
import Text.LaTeX.Packages.AMSMath (amsmath, math, to, quad)
import Text.LaTeX.Packages.Beamer
import Text.LaTeX.Packages.Inputenc



-- selectors for triples

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

-- selectors for quadruples

fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

trd4 :: (a, b, c, d) -> c
trd4 (_, _, c, _) = c

fth4 :: (a, b, c, d) -> d
fth4 (_, _, _, d) = d

-- map functions for quadruples

map4fst :: (a -> z) -> (a, b, c, d) -> (z, b, c, d)
map4fst f (a, b, c, d) = (f a, b, c, d)

map4snd :: (b -> z) -> (a, b, c, d) -> (a, z, c, d)
map4snd f (a, b, c, d) = (a, f b, c, d)

map4trd :: (c -> z) -> (a, b, c, d) -> (a, b, z, d)
map4trd f (a, b, c, d) = (a, b, f c, d)

map4fth :: (d -> z) -> (a, b, c, d) -> (a, b, c, z)
map4fth f (a, b, c, d) = (a, b, c, f d)

equalf :: Eq a => (t -> a) -> t -> t -> Bool
equalf f x y =
  f x == f y

mapAccumLFliped :: [x] -> acc -> (acc -> x -> (acc, y)) -> (acc, [y])
mapAccumLFliped f z l = mapAccumL l z f


-- grammar symbols

data Symbol
  = T { symbolName :: String }
  | N { symbolName :: String }
  deriving (Eq, Ord)

instance Show Symbol where
  showsPrec _ (T x) = showChar '"' . showString x . showChar '"'
  showsPrec _ (N x) = showString x

instance Texy Symbol where
  texy (T x) = textbf (texttt (fromString x))
  texy (N x) = textup (textsf (fromString x))

isTerminal :: Symbol -> Bool
isTerminal (T _) = True
isTerminal _     = False

isNTerminal :: Symbol -> Bool
isNTerminal (N _) = True
isNTerminal  _    = False

-- symbolName :: Symbol -> String
-- symbolName (T x) = x
-- symbolName (N x) = x


-- production rule
data Production = (::=) { lhs :: Symbol
                        , rhs :: [Symbol]
                        }
  deriving (Eq, Show)

infix 8 ::=

instance Texy Production where
  texy (lhs ::= rhs) =
    texy lhs <> math to <> mconcat (intersperse (hspace (Pt 2)) (map texy rhs))
-- type Production = [Symbol]

-- A grammar is given by a list of production rules.
newtype Grammar = Grammar [Production]

instance Show Grammar where
  show (Grammar g) = showG g
    where
    showG [] = ""
    showG ((nt::=rhs):rs) = width (show nt) len ++ "->" ++
                            foldr (\x s -> " " ++ show x ++ s) "" rhs ++
                            "\n" ++
                            showG rs

    len = maximum (map (length . show . lhs) g)

    width str n = str ++ " " ++ replicate (n - length str) '-'


-- instance Texy Grammar where
--   texy g = align_ $ map (\(i,(l:r)) ->
--                               uncover [FromSlide i] $
--                               texy l &
--                               do to
--                                  case r of
--                                    [] -> return ()
--                                    s:ss -> do texy s
--                                               foldr (\s z -> hspace_ (Em 0.5) >> texy s >> z) (return ()) ss)
--                            (zip [1..] g)


addEOFToGrammar :: Grammar -> Grammar
addEOFToGrammar grm@(Grammar rules) =
  case rules of
    (_ ::= [_, T "$"]) : _ -> grm
    (N s ::= _) : _ -> Grammar ((N newStart ::= [N s, T "$"]) : rules)
      where
        options = ["S", s ++ "'", "_" ++ s]
        newStart = fromMaybe "START" (find (flip (notElem . N) (nonterminals grm)) options)
    _ -> grm

startSymbol :: Grammar -> Maybe Symbol
startSymbol (Grammar ((lhs::=_) : _)) = Just lhs
startSymbol _                         = Nothing

terminals :: Grammar -> [Symbol]
terminals (Grammar productions) =
  unique (filter isTerminal (concatMap rhs productions))

nonterminals :: Grammar -> [Symbol]
nonterminals (Grammar productions) =
  unique (concatMap (\(lhs::=rhs) -> lhs : filter isNTerminal rhs) productions)

nullableSet :: Grammar -> [String]
nullableSet (Grammar rules) = converge f []
  where
    f nullables = unique (map (symbolName . lhs) (filter (all ((`elem` nullables) . symbolName) . rhs) rules))

isNullable :: [Symbol] -> [String] -> Bool
isNullable []       _   = True
isNullable (T _:_)  _   = False
isNullable (N x:xs) set = elem x set && isNullable xs set


firstSets :: Grammar -> [String] -> [(String, [String])]
firstSets (Grammar rules) nullables = converge f []
  where
    f firsts = foldl g firsts rules
      where
        g firsts (lhs::=rhs) = merge (symbolName lhs, first rhs nullables firsts) firsts

first :: [Symbol] -> [String] -> [(String, [String])] -> [String]
first [] _ _  = []
first (T x:_) _ _ = [x]
first (N x:xs) nullables firsts
  | elem x nullables = union first_x (first xs nullables firsts)
  | otherwise        = first_x
  where
    first_x = look x firsts

followSets :: Grammar -> [String] -> [(String, [String])] -> [(String, [String])]
followSets (Grammar grammar) nullables firsts = converge go []
    where
      go follows = foldl g follows grammar
        where
          g follows (N x::=xs) = inLhs (reverse xs) (inRhs xs follows)
            where
              inRhs [] fol = fol
              inRhs (N y:ys) fol = inRhs ys (merge (y,first ys nullables firsts) fol)
              inRhs (T _:ys) fol = inRhs ys fol
          
              inLhs [] fol = fol
              inLhs (T _:_) fol = fol
              inLhs (N y:ys) fol | elem y nullables = inLhs ys fol'
                                 | otherwise        = fol'
                where
                  fol' = merge (y, look x fol) fol


type LL1Table = [(String, [(String, [Symbol])])]

ll1Table :: Grammar -> LL1Table
ll1Table gr@(Grammar grammar) = go grammar []
    where
      go [] t = organizeTable t
      go ((N x::=xs):ps) t | isNullable xs nullables = go ps t2
                           | otherwise               = go ps t1
        where
          addActions ys table = foldr insert table (map (\y -> ((x,y),xs)) ys)
          t1 = addActions (first xs nullables firsts) t
          t2 = addActions (look x follows) t1

      nullables = nullableSet gr
      firsts = firstSets gr nullables
      follows = followSets gr nullables firsts


organizeTable :: [((String,String),[Symbol])] -> LL1Table
organizeTable table = map sepNT (groupBy itemEq (sort table))
    where
      itemEq ((x1,_),_) ((x2,_),_) = x1 == x2

      sepNT tab@(((nt,_),_):_) = (nt, map (\((_,t),rhs) -> (t,rhs)) tab)


showTable :: LL1Table -> String
showTable table = foldr f "" table
    where
      f (nt,prules) s = "\n\n<" ++ nt ++ "> ::=" ++ foldr g s prules
      g (t,rhs) s = "\n\t" ++ t ++ "\t==>\t" ++ foldr h s rhs
      h x s = show x ++ " " ++ s


---------------------------------------------------------------------
-- Auxiliary definitions
---------------------------------------------------------------------

converge :: (Eq a) => (a -> a) -> a -> a
converge f x
  | x == y = y
  | otherwise = converge f y
  where
    y = f x

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _    []     = []
takeUntil test (x:xs) = x : if test x
                            then takeUntil test xs
                            else []

insert :: Eq a => a -> [a] -> [a]
insert x set | elem x set = set
             | otherwise  = x : set

unique :: Eq a => [a] -> [a]
unique = foldr insert []

merge :: (Eq a, Eq b) => (a, [b]) -> [(a, [b])] -> [(a, [b])]
merge pair [] = [pair]
merge (key,val) ((k,v):rest) | key == k  = (key, union val v) : rest
                             | otherwise = (k,v) : merge (key,val) rest


look :: Eq a => a -> [(a, [b])] -> [b]
look key assoc = fromMaybe [] (lookup key assoc)


showAssocList :: (Show a, Show b) => [(a, [b])] -> String
showAssocList [] = ""
showAssocList ((x,ys):rest) = "\n" ++ show x ++ "\t:\t" ++
                              foldr (\t s -> show t ++ " " ++ s) "" ys ++
                              showAssocList rest


testGrammar :: Grammar -> IO ()
testGrammar g =
  do putStrLn ">>>>>>>>>> Nullable set: "
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


ltDoc :: Monad m => Grammar -> LaTeXT_ m
ltDoc (Grammar _g) =
  do documentclass [] beamer
     usepackage [utf8] inputenc
     usepackage [] amsmath
     usetheme CambridgeUS
     title "Nullable"
     document $ do frame maketitle
                   frame $ do frametitle "Exemplo"
                              -- texy g


