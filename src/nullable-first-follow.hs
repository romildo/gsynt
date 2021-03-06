{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}

module Main where

import Data.List (intersperse, intercalate, mapAccumL, partition)

import Control.Monad.Trans.State (execState, modify, get, put)
import Control.Monad (forM, forM_)

import Text.LaTeX
import Text.LaTeX.Base.Syntax (LaTeX(TeXRaw, TeXComm), TeXArg(FixArg, MSymArg))
import Text.LaTeX.Base.Class (LaTeXC, comm0, liftL)
import Text.LaTeX.Packages.AMSMath
import Text.LaTeX.Packages.AMSSymb
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.Beamer

import Debug.Trace (trace)

data Flag = Vis | Invis | Alert | Only | Uncover | BGYellow
          deriving (Eq,Show)

data Status = B | E | BE
            deriving (Eq,Show)

type Annotation = [(Int, Status, Flag)]

data An a = An { anAn   :: Annotation
               , anInfo :: a
               }
          deriving (Eq,Show)

findEnd x lst = go lst 0 []
  where
    go (k@(_,B,y):ks)   n acc | x == y = go ks (n+1) (k:acc)
    go ((slide,E,y):ks) 0 acc | x == y = Just (slide,reverse acc ++ ks)
    go (k@(_,E,y):ks)   n acc | x == y = go ks (n-1) (k:acc)
    go (k:ks)           n acc          = go ks n (k:acc)
    go []               _ _            = Nothing

attrsFun lst =
  attrsFun'' (attrsFun' lst)

attrsFun'' [] = id
attrsFun'' lst@((action, _) : _) =
  fun action (map snd lst1) . attrsFun'' lst2
  where
    (lst1, lst2) = partition ((== action) . fst) lst
    
    fun x = case x of
             Only     -> only
             Uncover  -> uncover
             Vis      -> visible
             Invis    -> invisible
             Alert    -> alert
             BGYellow -> bgyellow

attrsFun' [] = []
attrsFun' ((slide,pos,x):as) =
  (x, overlaySpec) : attrsFun' rest
  where
    (overlaySpec,rest) = case pos of
                          B  -> case findEnd x as of
                                 Just (slide',as') ->
                                   (FromToSlide slide slide', as')
                                 Nothing ->
                                   (FromSlide slide, as)
                          E  -> (ToSlide slide, as)
                          BE -> (OneSlide slide, as)

bgyellow :: LaTeXC l => [OverlaySpec] -> l -> l
bgyellow os = liftL $ \l -> TeXComm "bgyellow" [ MSymArg $ fmap (TeXRaw . render) os , FixArg l ]


instance Texy a => Texy (An a) where
  texy (An attrs x) = attrsFun attrs (texy x)

instance Functor An where
  fmap f (An an x) = An an (f x)

data Symbol
  = T String
  | N String
  deriving (Eq,Show)

isTerminal (T _) = True
isTerminal _     = False

isNTerminal (N _) = True
isNTerminal  _    = False

instance Texy Symbol where
  texy (T str) = texttt (textbf (fromString str))
  texy (N str) = textit (fromString str)

data View =
  View { step       :: Int
       , grammar    :: [ An [ An Symbol ] ]
       , nts        :: [ An Symbol ]
       , nullable   :: [ [ An Bool ] ]
       , first      :: [ [ An Symbol ] ]
       , follow     :: [ [ An Symbol ] ]
       , tableAn    :: Annotation
       , nullableAn :: Annotation
       , firstAn    :: Annotation
       , followAn   :: Annotation
       , expl       :: [ An String ]
       }
  deriving (Show)

foldrFlipped xs z f = foldr f z xs

getNts grm =
  foldrFlipped grm [] $ \prod nts ->
    foldrFlipped prod nts $ \s nts ->
      case s of
       N _ | not (elem s nts) -> s : nts
       _                      -> nts
          
tick =
  modify $ \view@View{step = i} ->
            view{step = i + 1}


explain msg =
  modify $ \view@View{step = i, expl = xs} ->
            view{expl = xs ++ [ An [ (i,B,Only), (i,E,Only) ] msg ]}

addAn i ans1 (An ans2 x) =
  An (ans2 ++ map (\(a,b) -> (i,a,b)) ans1) x


startView grm =
  View { step       = 1
       , grammar    = map (\prod -> An [] (map (An []) prod)) rules
       , nts        = map (An []) nts
       , nullable   = map (const []) nts
       , first      = map (const []) nts
       , follow     = map (const []) nts
       , tableAn    = []
       , nullableAn = []
       , firstAn    = []
       , followAn   = []
       , expl       = []
       }
  where
    (N start : _) : _ = grm
    newStart | elem (N "S") (getNts grm) = start ++ "'"
             | otherwise =  "S"
    rules = [N newStart, N start, T "$"] : grm
    nts = getNts rules

build grm =
  execState buildView (startView grm)

snoc x xs = xs ++ [x]

buildView = do
  -- uncover all productions, except the first one
  modify $ \v@View{step = i, grammar = p:ps} ->
    v{grammar = addAn i [(BE,Invis)] p : map (addAn i [(B,Uncover)]) ps}
  tick
  -- 
  -- uncover first production
  modify $ \v@View{step = i, grammar = p:ps} ->
    v{grammar = addAn i [(B,Uncover)] p : ps}
  -- alert first symbol in rhs of first production, and lhs of second production
  modify $ \v@View{step = i, grammar = p1:p2:ps} ->
    v{grammar = fmap (\(lhs:x:xs) -> lhs : addAn i [(BE,Alert)] x : xs) (addAn i [(BE,BGYellow)] p1)
                : fmap (\(lhs:rhs) -> addAn i [(BE,Alert)] lhs : rhs) (addAn i [(BE,BGYellow)] p2)
                : ps
     }
  explain "Nova regra introduzindo fim da entrada"
  tick
  --
  -- uncover table, nonterminals and nullable
  modify $ \v@View{step = i, tableAn} ->
    v{ tableAn = tableAn ++ [(i,B,Vis)] }
  tick
  --
  -- uncover nonterminals
  modify $ \v@View{step = i, nts} ->
    v{ nts = map (addAn i [(B,Vis)]) nts }
  tick
  --
  buildNullables
  buildFirsts
  buildFollows
  --
  tick
  explain "Fim"

-- setNts attrs =
--   modify $ \view@View{step = i, nts = nts} ->
--             view{nts = fmap (map (addAn i attrs)) nts}

-- setNullables attrs =
--   modify $ \view@View{step = i, nts = nts} ->
--             view{nts = fmap (map (fmap (map4snd (addAn i attrs)))) nts}

mapAccumLFliped f z l = mapAccumL l z f

buildNullables = do
  v@View{step = i, grammar, nts, nullable} <- get
  let nterms = map anInfo nts
  let nullable1 = map (An [(i,B,Vis),(i,BE,Alert)] False :) nullable
  let (i1, grammar1) =
        mapAccumLFliped grammar i $ \i x@(An a p@(_:rhs)) ->
          if any isTerminal (map anInfo rhs)
          then (i+1, An (a ++ [(i,E,Uncover)]) p)
          else (i, x)
  let ((i2, nullable2), grammar2) =
        mapAccumLFliped grammar1 (i1+1, nullable1) $ \(i, n) x ->
          case x of
           An a p@(An _ lhs : []) ->
             let Just (n1, y, n2) = lookup2 lhs nterms n
             in ((i+1, n1 ++ [y ++ [An [(i,B,Vis),(i,BE,Alert)] True]] ++ n2) , An (a ++ [(i,BE,BGYellow),(i,E,Uncover)]) p)
           _ ->
             ((i, n), x)
  let ((i3, nullable3), grammar3) =
        whileChange (\((_,x),_) ((_,y),_) -> x == y) ((i2, nullable2), grammar2) $ \((i, n), g) ->
          mapAccumLFliped g (i, n) $ \(i, n) x ->
           case x of
            An a p@(An _ lhs : rhs@(_:_))
              | all (isNullable nterms n . anInfo) rhs ->
                  let Just (n1, y, n2) = lookup2 lhs nterms n
                  in ((i+1, n1 ++ [y ++ [An [(i,B,Vis),(i,BE,Alert)] True]] ++ n2) , An (a ++ [(i,BE,BGYellow),(i,E,Uncover)]) p)
              | all (isNTerminal . anInfo) rhs ->
                  ((i+1, n) , An (a ++ [(i,BE,BGYellow)]) p)
            _ ->
              ((i, n), x)
  let grammar4 =
        flip map grammar3 $ \p@(An a x) ->
          case last a of
           (_,E,Uncover) -> An (a ++ [(i3,B,Uncover)]) x
           _ -> p
  put v{step = i3+2, grammar = grammar4, nullable = nullable3}

buildFirsts = do
  v@View{step = i, grammar, nts, nullable, first} <- get
  let nterms = map anInfo nts
  let nullableSet = map (anInfo . fst) (filter (anInfo . last . snd) (zip nts nullable))
  let ((i1, first1), grammar1) =
        mapAccumLFliped grammar (i,first) $ \(i, f) x@(An a p@(An _ lhs : rhs)) ->
          case rhs of
           An _ t@(T _) : _ ->
             let Just (f1, y, f2) = lookup2 lhs nterms f
             in ((i+1, f1 ++ [merge y [An [(i,B,Vis),(i,BE,Alert)] t]] ++ f2), An (a ++ [(i,BE,BGYellow),(i,E,Uncover)]) p)
           [] ->
             ((i+1, f), An (a ++ [(i,BE,BGYellow),(i,E,Uncover)]) p)
           _ ->
             ((i, f), x)
  let ((i2, first2), grammar2) =
        whileChange (\((_,x),_) ((_,y),_) -> map (map anInfo) x == map (map anInfo) y) ((i1, first1), grammar1) $ \((i, f), g) ->
          mapAccumLFliped g (i, f) $ \(i, f) x@(An a p@(An _ lhs : rhs)) ->
            let loop (An _ t@(N _) : rest) i f x = 
                  let Just (f1, y, f2) = lookup2 lhs nterms f
                      Just (_, z0, _) = lookup2 t nterms f
                      z = map (An [(i,B,Vis),(i,BE,Alert)] . anInfo) z0
                      i' = i + 1
                      f' = f1 ++ [merge y z] ++ f2
                      x' = addAn i [(BE,BGYellow)] x
                  in if elem t nullableSet
                     then loop rhs i' f' x'
                     else ((i', f'), x')
                loop _ i f p = ((i, f), x)
            in loop rhs i f x
  let grammar3 =
        flip map grammar2 $ \p@(An a x) ->
          case last a of
           (_,E,Uncover) -> An (a ++ [(i2,B,Uncover)]) x
           _ -> p
  put v{step = i2+2, grammar = grammar3, first = first2}

buildFollows = do
  return ()
  -- v@View{step = i, grammar, nts, nullable, first, follow} <- get
  -- let nterms = map anInfo nts
  -- let nullableSet = map (anInfo . fst) (filter (anInfo . last . snd) (zip nts nullable))
  -- let firstSets = zip (map anInfo nts) (map (map anInfo) first)
  -- let ((i1, follow1), grammar1) =
  --       mapAccumLFliped grammar (i,follow) $ \(i, f) x@(An a p@(_ : rhs)) ->
  --         let loop [] i f x =
  --               ((i, f), addAn i [(E,BGYellow)] x)
  --             loop (An _ (T _) : rest) i f x =
  --               loop rest i f x
  --             loop (An _ k@(N _) : rest) i f x =
  --               let Just (f1, y, f2) = lookup2 k nterms f
  --                   loop2 [] i y x =
  --                     loop rest i (f1 ++ [y] ++ f2) x
  --                   loop2 ((An _ t@(T _)) : _) i y x =
  --                     let z = [ An [(i,B,Vis),(i,BE,Alert)] t ]
  --                         i' = i + 1
  --                         f' = f1 ++ [merge y z] ++ f2
  --                     in loop rest i' f' x
  --                   loop2 ((An _ t@(N _)) : ts) i newf x =
  --                     loop rest i (f1 ++ [newf] ++ f2) x
  --                     -- let Just z0 = lookup t firstSets
  --                     --     z = map (An [(i,B,Vis),(i,BE,Alert)]) z0
  --                     --     i' = i + 1
  --                     --     newf' = merge y z
  --                     -- in if elem t nullableSet
  --                     --    then loop2 ts i' newf' x
  --                     --    else loop rest i' (f1 ++ [merge y newf'] ++ f2) x
  --               in loop2 rest i y x
  --         in loop rhs i f (addAn i [(B,BGYellow)] x)
  -- put v{step = i1, grammar = grammar1, follow = follow1}

merge [] ys = ys
merge xs [] = xs
merge (x@(An a p):xs) ys = case break (\(An _ q) -> p == q) ys of
                            (ys1, An b _ : ys2) -> An (a ++ b) p : merge xs (ys1 ++ ys2)
                            _ -> x : merge xs ys

lookupIndex [] _ _ = Nothing
lookupIndex (index:indexes) (x:xs) key | key == index = Just x
                                       | otherwise = lookupIndex indexes xs key

isNullable nterms lst nterm = case lookupIndex nterms lst nterm of
                               Just ks@(_:_) -> anInfo (last ks)
                               _ -> False

whileChange test x f
  | test x y = y
  | otherwise = whileChange test y f
  where
    y = f x


lookup2 x lst1 lst2 = lookup2' lst1 lst2 []
  where
    lookup2' [] [] _ = Nothing
    lookup2' (y:ys) (z:zs) acc | x == y = Just (reverse acc, z, zs)
                               | otherwise = lookup2' ys zs (z:acc)

splitByNt _ [] _ = ([], [])
splitByNt x (y:ys) zs'@(z:zs) | x == y = ([], zs')
                              | otherwise = let (zs1,zs2) = splitByNt x ys zs
                                            in (zs1 ++ [z], zs2)


texyView :: Monad m => View -> LaTeXT_ m
texyView v =
  do tabular (Just Top) [LeftColumn,LeftColumn] $ texyViewGrammar v & texyViewNFF v
     par
     texyViewExplain v

texyViewGrammar :: Monad m => View -> LaTeXT_ m
texyViewGrammar v@View{grammar} = do
  let names = intercalate "," (map (\(An _ (An _ lhs : _)) -> case lhs of N x -> x; T x -> x) grammar)
  raw "\\newdimen\\largest"
  raw "\\widest("
  raw (fromString names)
  raw ")\\largest"
  tabular (Just Top) [LeftColumn] $ do
    sequence_ $ intersperse lnbk $ flip map grammar $ \(An a (lhs : rhs)) ->
      attrsFun a $ do
        makebox (Just (CustomMeasure (raw "\\largest"))) (Just HLeft) (texy lhs)
        hspace (Pt 1) <> math to <> hspace (Pt 1)
        sequence_ (intersperse (hspace (Pt 2)) (map texy rhs))

texyViewNFF :: Monad m => View -> LaTeXT_ m
texyViewNFF v@View{nts, nullable, first, follow, tableAn, nullableAn, firstAn, followAn} =
  attrsFun tableAn $ do
    tabular
      (Just Top)
      [LeftColumn
      ,LeftColumn
      ,LeftColumn
      ,LeftColumn
      ] $
      do "NT" & "Nullable" & "First" & "Follow"
         lnbk
         hline
         (forM_ (zip4 nts nullable first follow) $ \(nt, nullable, first, follow) ->
           do texy nt
               & sequence_
                   (intersperse
                      (raw "~")
                      (flip map nullable $ \(An a x) ->
                        attrsFun a (math (if x then checkmark else comm0 "times"))))
               & sequence_ (intersperse (raw "~") (map texy first))
               & mapM_ texy follow
              lnbk
              hline
          )

texyViewExplain :: Monad m => View -> LaTeXT_ m
texyViewExplain View{expl} =
  return ()
  -- overprint $ center $  do
  --   forM_ expl $ \ (An attrs text) ->
  --     attrsFun attrs (fromString text)
  --   return ()

zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds
zip4 _ _ _ _ = []



myGRM = [ [ N "A", N "Bona", N "C" ]
        , [ N "A", T "w", N "A", T "z" ]
        , [ N "Bona", T "x" ]
        , [ N "C" ]
        , [ N "C", N "A" ]
        ]

g_3_12 =
  [ [ N "Z", T "d"               ]
  , [ N "Z", N "X", N "Y", N "Z" ]
  , [ N "Y"                      ]
  , [ N "Y", T "c"               ]
  , [ N "X", N "Y"               ]
  , [ N "X", T "a"               ]
  ]

g_3_15 =
  [ [ N "E",  N "T", N "E'"         ]
  , [ N "E'", T "+",  N "T", N "E'" ]
  , [ N "E'", T "-",  N "T", N "E'" ]
  , [ N "E'"                        ]
    
  , [ N "T",  N "F", N "T'"         ]
  , [ N "T'", T "*",  N "F", N "T'" ]
  , [ N "T'", T "/",  N "F", N "T'" ]
  , [ N "T'"                        ]
    
  , [ N "F",  T "id"                ]
  , [ N "F", T "num"                ]
  , [ N "F", T "(",  N "E", T ")"   ]
                                    ]


main :: IO ()
main = do print (build g_3_12)
          execLaTeXT simple >>= renderFile "simple.tex"

simple :: Monad m => LaTeXT_ m
simple = do
  thePreamble
  document theBody

thePreamble :: Monad m => LaTeXT_ m
thePreamble = do
  documentclass [] beamer
  usepackage [utf8] inputenc
  usepackage [] amssymb
  raw "\\usepackage{widest}"
  raw "\\newcommand<>{\\bgyellow}[1]{{\\fboxsep 0pt\\only#2{\\colorbox{yellow!30}}{#1}}}"
  usetheme CambridgeUS
  setbeamercovered [Transparent Nothing]
  author "José Romildo Malaquias"
  title "Nullable, First and Follow"

theBody :: Monad m => LaTeXT_ m
theBody = do
  frame maketitle
  frame $ do
    frametitle "Example"
    texyView (build g_3_12)
