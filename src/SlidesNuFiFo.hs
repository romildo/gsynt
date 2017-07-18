{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE ExistentialQuantification #-}

module SlidesNuFiFo where

import Data.List (intersperse, intercalate, sort, inits, tails, union)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Text.Printf (printf)
import Control.Monad (forM_)
import Debug.Trace (trace, traceShowId)
import IPPrint (pshow, pprint)

import Text.LaTeX
import Text.LaTeX.Base.Class (LaTeXC, comm0, fromLaTeX)
import Text.LaTeX.Packages.AMSMath (text, math, to, epsilon, cup, thinspace, quad)
import Text.LaTeX.Packages.AMSSymb (amssymb, checkmark)
import Text.LaTeX.Packages.Beamer (beamer, frame, frametitle, CoverOption(Invisible, Transparent), setbeamercovered, overprint)
import Text.LaTeX.Packages.Inputenc (inputenc, utf8)
import Text.LaTeX.Packages.Color (ColSpec(..), Color(..), textcolor)
import Text.LaTeX.Packages.TikZ (tikz)

-- import Debug.Trace (trace)

import Grammar (Symbol(..), Production(..), Grammar(..), isTerminal, isNTerminal, terminals, nonterminals, mapAccumLFliped, equalf, addEOFToGrammar, snd3, trd4)
import Doc (Doc(..), Slide, Effects, EffectName(..), RangeIndicator(..), addDoc, applyEffects)

data View =
  View
  { step        :: Int
  , grammar     :: [ Doc [ Doc Symbol ] ]
  , nts         :: [ Doc Symbol ]
  , nullable    :: [ [ Doc Bool ] ]
  , first       :: [ [ Doc Symbol ] ]
  , follow      :: [ [ Doc Symbol ] ]
  , tableLL1    :: [ ((Symbol, Symbol), Doc [ Symbol ] ) ]
  , tableDoc    :: Effects
  , nullableDoc :: Effects
  , firstDoc    :: Effects
  , followDoc   :: Effects
  , tableLL1Doc :: Effects
  , expl        :: [ Doc LaTeX ]
  , ts          :: [ Symbol ]
  , nonts       :: [ Symbol ]
  }
--  deriving (Show)


tracePshowId x = trace (pshow x) x


foldrFlipped :: [a] -> b -> (a -> b -> b) -> b
foldrFlipped xs z f = foldr f z xs

foldlFlipped :: [a] -> b -> (b -> a -> b) -> b
foldlFlipped xs z f = foldl f z xs


foldlUntil :: (a -> Bool) -> (b -> a -> b) -> b -> [a] -> b
foldlUntil _ _ z [] = z
foldlUntil test f z (x:xs) | test x = z'
                           | otherwise = foldlUntil test f z' xs
  where
    z' = f z x

foldlUntilFlipped :: (a -> Bool) -> [a] -> b -> (b -> a -> b) -> b
foldlUntilFlipped test list z f = foldlUntil test f z list

splits :: [a] -> [([a], [a])]
splits list = zip (inits list) (tails list)


sepBy :: Monoid a => a -> [a] -> a
sepBy sep =
  mconcat . intersperse sep

sepBeginBy :: Monoid c => c -> [c] -> c
sepBeginBy sep =
  mconcat . fmap (sep <>)

sepEndBy :: Monoid c => c -> [c] -> c
sepEndBy sep =
  mconcat . fmap (<> sep)

brackets :: Monoid c => c -> c -> c -> c
brackets open close x =
  open <> x <> close

set :: LaTeXC l => l -> l
set =
  brackets ("{" <> thinspace) (thinspace <> "}")


addExplanation :: [ Doc LaTeX ] -> Int -> LaTeX -> [ Doc LaTeX ]
addExplanation expl i text = expl ++ [Doc [(Only,BE,i)] text]

startView :: Grammar -> View
startView (Grammar productions) =
  View
  { step        = 1
  , grammar     = map (Doc [] . map (Doc [])) productions'
  , nts         = map (Doc []) nts
  , nullable    = map (const []) nts
  , first       = map (const []) nts
  , follow      = map (const []) nts
  , tableLL1    = []
  , tableDoc    = []
  , nullableDoc = []
  , firstDoc    = []
  , followDoc   = []
  , tableLL1Doc = []
  , expl        = []
  , ts          = sort ts
  , nonts       = nts
  }
  where
    productions' = map (\(lhs::=rhs) -> lhs : rhs) productions
    nts = nonterminals (Grammar productions)
    ts = terminals (Grammar productions)

build :: Grammar -> View
build grm =
  buildView (startView (addEOFToGrammar grm))

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

buildView :: View -> View
buildView v =
  buildTableLL1 $
  buildFollows $
  buildFirsts $
  buildNullables $
  showGrammar $
  v

showGrammar :: View -> View
showGrammar v@View{step = i, grammar = Doc a (s1:s2:ss) : ps, nts, tableDoc, expl} = v'
  where
  -- uncover all productions, except the first one
  -- and then uncover first production
  -- and then alert first symbol in rhs of first production,
  -- and the lhs of second production
  r1 = Doc (a ++ [(Invis,B,i),(Invis,E,i+1), (Uncover,B,i+2), (Shade2,BE,i+2)])
           (s1 : addDoc [(Alert,BE,i+2)] s2 : ss)
  Doc b (Doc c t1:ts) : rest = map (addDoc [(Uncover,B,i)]) ps
  r2 = Doc (b ++ [(Shade,B,i+1),(Shade,E,i+2)]) (Doc (c ++ [(Alert,B,i+1),(Alert,E,i+2)]) t1 : ts)
  grammar' = r1 : r2 : rest
  -- uncover table, nonterminals and nullable
  tableDoc' = tableDoc ++ [(Vis,B,i+3)]
  -- uncover nonterminals
  nts' = map (addDoc [(Vis,B,i+4)]) nts
  expl' = expl ++
          [ Doc [(Only,BE,i)]
            (center $
              textbf "Esta é a gramática livre de contexto!" <> par)
          , Doc [(Only,B,i+1),(Only,E,i+2)]
            (center $
              textbf "Acertando a gramática" <> par <>
              "Símbolo inicial: " <> textcolor (DefColor Red) (texy s2) <> par <>
              "Novo símbolo inicial: " <> textcolor (DefColor Red) (texy s1) <> par
            )
          , Doc [(Only,BE,i+2)]
            (center $
              "Nova regra de produção: " <> texy (info s1 ::= [info s2, T "$"]) <> par)
          , Doc [(Only,B,i+3),(Only,E,i+4)]
            (center $
              textbf "Tabela de símbolos anuláveis, conjuntos first e conjuntos follow" <> par)
          ]
  v' = v{ step = i+5, grammar = grammar', nts = nts', tableDoc = tableDoc', expl = expl'}

buildNullables :: View -> View
buildNullables v@View{step = i, grammar, nts, nullable, expl} = v'
  where
  nterms = map info nts
  nullable1 = map (Doc [(Vis,B,i),(Alert,BE,i)] False :) nullable
  expl0 = addExplanation expl i $
            center $
              textbf "Encontrando os símbolos não terminais anuláveis" <>
              par <>
              "Inicialmente nenhum símbolo não terminal é anulável"

  -- rules with terminals in their rhs
  ((i1, expl1), grammar1) =
    mapAccumLFliped grammar (i+1, expl0) $ \(i, e) prod@(Doc a (lhs:rhs)) ->
      if any (isTerminal . info) rhs
      then let rhs' = map (\(Doc b k) ->
                             if isTerminal k
                             then Doc (b ++ [(Alert,BE,i)]) k
                             else Doc b k)
                          rhs
               prod' = Doc (a ++ [(Shade,BE,i),(Uncover,E,i)]) (lhs:rhs')
               e' = addExplanation e i (center "Regra com terminal no lado direito é inútil")
           in ((i+1, e'), prod')
      else ((i, e), prod)

  -- rules with empty rhs
  ((i2, nullable2, expl2), grammar2) =
    mapAccumLFliped grammar1 (i1, nullable1, expl1) $ \(i, n, e) prod ->
      case prod of
       Doc _ [Doc _ lhs] -> ((i+1, n', e'), prod')
         where
           Just (n1, y, n2) = lookup2 lhs nterms n
           z = [Doc [(Vis,B,i),(Alert,BE,i),(Shade,BE,i)] True]
           n' = n1 ++ [y ++ z] ++ n2
           prod' = addDoc [(Shade,BE,i),(Uncover,E,i)] prod
           e' = addExplanation e i (center "Lado direito da regra é vazio")
       _ -> ((i, n, e), prod)

  -- rules with nonempty rhs without terminals
  ((i3, nullable3, expl3), grammar3) =
    whileChange (equalf (snd3 . fst)) ((i2, nullable2, expl2), grammar2) $ \((i, n, e), g) ->
      mapAccumLFliped g (i, n, e) $ \(i, n, e) prod ->
        case prod of
          Doc a (Doc b lhs : rhs@(_:_))
            | any (isTerminal . info) rhs ->
                ((i, n, e), prod)
            | isNullable nterms n lhs ->
                ((i, n, e), prod)
            | all (isNullable nterms n . info) rhs ->
                let Just (n1, y, n2) = lookup2 lhs nterms n
                    z = [Doc [(Vis,B,i),(Alert,BE,i),(Shade,BE,i)] True]
                    n' = n1 ++ [y ++ z] ++ n2
                    prod' = addDoc [(Shade,BE,i)] prod
                    e' = addExplanation e i (center "Lado direito da regra é anulável")
                in ((i+1, n', e'), prod')
            | otherwise ->
                let rhs' = map (\(Doc c k) ->
                                  if isNullable nterms n k
                                  then Doc c k
                                  else Doc (c ++ [(Alert,BE,i)]) k)
                               rhs
                    prod' = Doc (a ++ [(Shade,BE,i)]) (Doc b lhs : rhs')
                    e' = addExplanation e i (center "Lado direito da regra não é anulável")
                in ((i+1, n, e'), prod')
          _ -> ((i, n, e), prod)

  -- Uncover all covered productions
  grammar4 =
    flip map grammar3 $ \p@(Doc a x) ->
      case last a of
       (Uncover,E,_) -> Doc (a ++ [(Uncover,B,i3)]) x
       _ -> p
  v' = v{step = i3, grammar = grammar4, nullable = nullable3, expl = expl3}

buildFirsts :: View -> View 
buildFirsts v@View{step = i, grammar, nts, nullable, first, expl} = v'
  where
    nterms = map info nts
    nullableSet = map (info . fst) (filter (info . last . snd) (zip nts nullable))
    expl0 = addExplanation expl i $
            center $
              textbf "Encontrando os conjuntos first" <>
              par <>
              "Inicialmente são vazios"
    ((i1, nts1, first1, expl1), grammar1) =
      mapAccumLFliped grammar (i+1,nts,first,expl0) $ \(i, n, f, e) x@(Doc a p@(Doc _ lhs : rhs)) ->
        case rhs of
         Doc _ t@(T _) : _ ->
           let Just (f1, y, f2) = lookup2 lhs nterms f
               (nts_1, Doc b _ : nts_2) = break ((==lhs) . info) n
               n' = nts_1 ++ (Doc (b ++ [(Shade,BE,i)]) lhs : nts_2)
               e' = e ++ [ Doc [(Only,BE,i)] (explainFirst1 lhs (map info rhs) (map info y)) ]
           in ( (i+1, n', f1 ++ [mergeDoc y [Doc [(Vis,B,i),(Alert,BE,i)] t]] ++ f2, e')
              , Doc (a ++ [(Shade,BE,i),(Uncover,E,i)]) p
              )
         [] ->
           ( (i+1, n, f, e')
           , Doc (a ++ [(Shade,BE,i),(Uncover,E,i)]) p
           )
           where
             e' = e ++ [Doc [(Only,BE,i)] (explainFirst2 lhs)]
         _ ->
           ((i, n, f, e), x)
    ((i2, nts2, first2, expl2), grammar2) =
      whileChange (equalf (map (map info) . trd4 . fst)) ((i1, nts1, first1, expl1), grammar1) $ \((i, n, f, e), g) ->
        mapAccumLFliped g (i, n, f, e) $ \(i, n, f, e) x@(Doc a p@(Doc _ lhs : rhs)) ->
          let loop rhs1 (Doc _ t@(N _) : rest) i n f e x
                | elem t nullableSet = loop (t:rhs1) rest i' n' f' e' x'
                | otherwise = ((i', n', f', e'), x')
                where
                  Just (f1, y, f2) = lookup2 lhs nterms f
                  Just (_, z0, _) = lookup2 t nterms f
                  z = map (Doc [(Vis,B,i),(Alert,BE,i)] . info) z0
                  i' = i + 1
                  f' = f1 ++ [mergeDoc y z] ++ f2
                  x' = addDoc [(Shade,BE,i)] x
                  (nts_1, Doc b _ : nts_2) = break ((==lhs) . info) n
                  n' = nts_1 ++ (Doc (b ++ [(Shade,BE,i)]) lhs : nts_2)
                  e' = e ++ [Doc [(Only,BE,i)] (explainFirst3 lhs (reverse rhs1) t (map info rest) (map info y) (map info z0))]
              loop _ _ i n f e x = ((i, n, f, e), x)
          in loop [] rhs i n f e x
    grammar3 =
      flip map grammar2 $ \p@(Doc a x) ->
        case last a of
         (Uncover,E,_) -> Doc (a ++ [(Uncover,B,i2)]) x
         _ -> p
    v' = v{step = i2+2, grammar = grammar3, nts = nts2, first = first2, expl = expl2 }

explainFirst1 :: (LaTeXC l) => Symbol -> [Symbol] -> [Symbol] -> l
explainFirst1 lhs (t:rhs) set =
  center $
    raw "\\begin{tikzpicture}%\n" <>
    raw (fromString (printf "\\node[rectangle,draw,fill=yellow!30](%s){" a)) <>
    texy lhs <>
    raw "};%\n" <>
    raw (fromString (printf "\\node[][right of=%s](b){$\\to$};%%\n" a)) <>
    raw (fromString (printf "\\node[rectangle,draw,fill=green!30][right of=%s](%s){" b c)) <>
    texy t <>
    raw (fromString (printf "} edge [->, bend right=45] node[auto,swap]{FIRST} (%s);%%\n" a)) <>
    mconcat (map (\(a,b,x) -> raw (fromString (printf "\\node[][right of=%s](%s){" a b)) <> texy x <> raw "};%\n") bs) <>
    raw (fromString (printf "\\node[red,below] at (%s.south){símbolo terminal};%%\n" c)) <>
    raw "\\end{tikzpicture}%\n" <>
    par <>
    raw "$\\text{FIRST}(\\text{" <> texy lhs <> raw "}) = \\{\\," <> set' <> raw "\\,\\} \\cup \\{\\, \\text{" <> textcolor (DefColor Red) (texy t) <> raw "} \\,\\}$%%\n" <>
    raw "\n"
  where
    a : b : (names' @ (c : names'')) = map (:[]) ['a' ..]
    bs = zip3 names' names'' rhs
    set' = mconcat (intersperse (raw ",") (map texy set))

explainFirst2 :: LaTeXC l => Symbol -> l
explainFirst2 lhs =
  center $
    texy (lhs ::= []) <>
    par <>
    "O lado direito da regra é " <>
    math epsilon <>
    par <>
    "Logo a regra é inútil" <>
    raw "\n"

explainFirst3 :: (LaTeXC l) => Symbol -> [Symbol] -> Symbol -> [Symbol] -> [Symbol] -> [Symbol] -> l
explainFirst3 lhs rhs1 t rhs2 set1 set2 =
  center $
    raw "\\begin{tikzpicture}%\n" <>
    raw (fromString (printf "\\node[rectangle,draw,fill=yellow!30](%s){" a)) <>
    texy lhs <>
    raw "};%\n" <>
    raw (fromString (printf "\\node[][right of=%s](b){$\\to$};%%\n" a)) <>
    mconcat (map (\(a,b,x) ->
                    raw (fromString (printf "\\node[color=gray][right of=%s](%s){" a b)) <>
                    texy x <>
                    raw "};%\n"
                 )
              symbols1) <>
    raw (fromString (printf "\\node[rectangle,draw,fill=green!30][right of=%s](%s){" p q)) <>
    texy t <>
    raw (fromString (printf "} edge [->, bend right=45] node[auto,swap]{FIRST} (%s);%%\n" a)) <>
    mconcat (map (\(a,b,x) ->
                    raw (fromString (printf "\\node[][right of=%s](%s){" a b)) <>
                    texy x <>
                    raw "};%\n"
                 )
              symbols2) <>
    raw (fromString (printf "\\node[red,below] at (%s.south){símbolo não terminal};%%\n" q)) <>
    raw "\\end{tikzpicture}%\n" <>
    par <>
    raw "$\\text{FIRST}(\\text{" <>
    texy lhs <>
    raw "}) = \\{\\," <>
    set1' <>
    raw "\\,\\} \\cup \\{\\, \\text{" <>
    textcolor (DefColor Red) set2' <>
    raw "} \\,\\}$%%\n" <>
    raw "\n"
  where
    a : b : names = map (:[]) ['a' ..]
    (names1, q : names2) = splitAt (length rhs1) names
    p | null rhs1 = b
      | otherwise = last names1
    symbols1 = zip3 (b : names1) names1 rhs1
    symbols2 = zip3 (q : names2) names2 rhs2
    set1' = mconcat (intersperse (raw ",") (map texy set1))
    set2' = mconcat (intersperse (raw ",") (map texy set2))

buildFollows :: View -> View
buildFollows v@View{step, grammar, nts, nullable, first, follow, expl} = v'
  where
    nterms = map info nts
    nullableSet = map (info . fst) (filter (info . last . snd) (zip nts nullable))
    firstSets = zip (map info nts) (map (map info) first)
    --
    -- looking for non terminals followed by other symbols
    --
    ((i1, follow1, expl1), grammar1) =
      mapAccumLFliped grammar (step,follow,expl) $ \(i, f, e) prod@(Doc _ (lhs : rhs)) ->
        let lhs_ = info lhs
            rhs_ = map info rhs
            (ii, ff, ee) =
              case breakAll isNTerminal rhs_ of
                [] -> (i+1, f, e')
                  where
                    e' = addExplanation e i (explainFollows1 lhs_ rhs_)

                rhs' ->
                  foldlFlipped rhs' (i,f,e) $ \(i,f,e) (rhs1,k:rhs2) ->
                    case rhs2 of
                      [] -> (i', f, e')
                        where
                          i' = i + 1
                          e' = addExplanation e i (explainFollows2 lhs_ rhs1 k)
                          
                      _ ->
                        foldlUntilFlipped (\(as,bs) -> case bs of w:_ -> notElem w nullableSet; _ -> True) (splits rhs2) (i,f,e) $ \(i,f,e) (rhs2a,rhs2b) ->
                          case rhs2b of
                            [] -> (i, f, e)

                            next:rhs2b' -> (i+1, f', e')
                              where
                                Just (f1, y, f2) = lookup2 k nterms f
                                firstOfNext = case next of
                                                T _ -> [next]
                                                N _ -> fromJust (lookup next firstSets)
                                z = map (Doc [(Vis,B,i),(Alert,BE,i)]) firstOfNext
                                f' = f1 ++ [mergeDoc y z] ++ f2
                                e' = addExplanation e i (explainFollows3 lhs_ rhs1 k rhs2a next rhs2b' (map info y) firstOfNext)

            uncover_ = case reverse rhs_ of
                         N _ : _ -> []
                         _ -> [(Uncover,E,ii-1)]
            prod__ = addDoc ((Shade,B,i):(Shade,E,ii-1):uncover_) prod
        in
          ((ii, ff, ee), prod__)
    --
    -- looking for non terminals at the end of the rule
    --
    ((i2, follow2, expl2), grammar2) =
      whileChange (equalf (map (map info) . snd3 . fst)) ((i1, follow1, expl1), grammar1) $ \((i, f, e), g) ->
        mapAccumLFliped g (i, f, e) $ \(i, f, e) prod@(Doc _ (Doc _ lhs : rhs)) ->
          case map info (reverse rhs) of
            rhs'@(N _ : _) -> rhsLoop [] rhs' i f e (addDoc [(Shade,B,i)] prod)
              where
                Just (_, anFlwLhs, _) = lookup2 lhs nterms f
                flwLhs = map info anFlwLhs
                rhsLoop rhs1 (k@(N _) : rest) i f e p
                  | elem k nullableSet =
                      rhsLoop (k:rhs1) rest i' f' e' p
                  | otherwise =
                      ((i', f', e'), addDoc [(Shade,E,i)] p)
                  where
                    Just (f1, y, f2) = lookup2 k nterms f
                    z = map (Doc [(Vis,B,i),(Alert,BE,i)]) flwLhs
                    f' = f1 ++ [mergeDoc y z] ++ f2
                    i' = i + 1
                    e' = addExplanation e i (explainFollows4 lhs (reverse rest) k rhs1 (map info y) flwLhs)
                rhsLoop _ _ i f e p =
                  ((i+1, f, e), addDoc [(Shade,E,i)] p)

            _ -> ((i, f, e), prod)

    grammar3 =
      flip map grammar2 $ \p@(Doc a x) ->
        case last a of
         (Uncover,E,_) -> Doc (a ++ [(Uncover,B,i2)]) x
         _ -> p
    v' = v{step = i2, grammar = grammar3, follow = follow2, expl = expl2}

explainFollows1 :: (LaTeXC l) => Symbol -> [Symbol] -> l
explainFollows1 lhs rhs =
  center $
    texy (lhs ::= rhs) <> par <>
    "Nenhum símbolo não terminal no lado direito da regra " <> par <>
    "Regra inútil"

explainFollows2 :: (LaTeXC l) => Symbol -> [Symbol] -> Symbol -> l
explainFollows2 lhs rhs1 nt =
  center $
    raw "\\begin{tikzpicture}%\n" <>
    raw (fromString (printf "\\node[](%s){" lhs_)) <>
    texy lhs <>
    raw "};%\n" <>
    raw (fromString (printf "\\node[][right of=%s](%s){$\\to$};%%\n" lhs_ to_)) <>
    mconcat (map (\(a,b,x) ->
                    raw (fromString (printf "\\node[][right of=%s](%s){" a b)) <>
                    texy x <>
                    raw "};%\n")
              rhs1_) <>
    raw (fromString (printf "\\node[rectangle,draw,fill=yellow!30][right of=%s](%s){"
                      before_nt_ nt_)) <>
    texy nt <>
    raw "};%\n" <>
    raw "\\end{tikzpicture}%\n" <>
    par <>
    "Náo há nenhum símbolo após o último não terminal" <>
    raw "%\n"
  where
    lhs_ : names1 @ (to_ : names2) = map (:[]) ['a' ..]
    rhs1_ = zip3 names1 names2 rhs1
    before_nt_ : nt_ : _ = drop (length rhs1) names1

explainFollows3 :: (LaTeXC l) => Symbol -> [Symbol] -> Symbol -> [Symbol] -> Symbol -> [Symbol] -> [Symbol] -> [Symbol] -> l
explainFollows3 lhs rhs1 nt rhs2a next rhs2b set1 set2 =
  center $
    raw "\\begin{tikzpicture}%\n" <>
    raw (fromString (printf "\\node[](%s){" lhs_)) <>
    texy lhs <>
    raw "};%\n" <>
    raw (fromString (printf "\\node[][right of=%s](%s){$\\to$};%%\n" lhs_ to_)) <>
    mconcat (map (\(a,b,x) ->
                    raw (fromString (printf "\\node[][right of=%s](%s){" a b)) <>
                    texy x <>
                    raw "};%\n")
              rhs1_) <>
    raw (fromString (printf "\\node[rectangle,draw,fill=yellow!30][right of=%s](%s){"
                      (if null rhs1 then to_ else (snd3 (last rhs1_)))
                      nt_)) <>
    texy nt <>
    raw "};%\n" <>
    mconcat (map (\(a,b,x) ->
                    raw (fromString (printf "\\node[][right of=%s](%s){" a b)) <>
                    texy x <>
                    raw "};%\n")
              rhs2a_) <>
    raw (fromString (printf "\\node[rectangle,draw,fill=green!30][right of=%s](%s){"
                      (if null rhs2a then nt_ else (snd3 (last rhs2a_)))
                      next_)) <>
    texy next <>
    raw (fromString (printf "} edge [->, bend right=45] node[auto,swap]{FIRST}(%s);%%\n" nt_)) <>
    mconcat (map (\(a,b,x) ->
                    raw (fromString (printf "\\node[][right of=%s](%s){" a b)) <>
                    texy x <>
                    raw "};%\n")
              rhs2b_) <>
    raw "\\end{tikzpicture}%\n" <>
    par <>
    raw "$\\text{FOLLOW}(\\text{" <>
    texy nt <>
    raw "}) = \\{\\," <>
    set1' <>
    raw "\\,\\} \\cup \\{\\, \\text{" <>
    textcolor (DefColor Red) set2' <>
    raw "} \\,\\}$%%\n" <>
    raw "%\n"
  where
    lhs_ : names1 @ (to_ : names2) = map (:[]) ['a' ..]
    rhs1_ = zip3 names1 names2 rhs1
    names3@(nt_ : names4) = drop (length rhs1) names2
    rhs2a_ = zip3 names3 names4 rhs2a
    names5@(next_ : names6) = drop (length rhs2a) names4
    rhs2b_ = zip3 names5 names6 rhs2b
    set1' = mconcat (intersperse (raw ",") (map texy set1))
    set2' = mconcat (intersperse (raw ",") (map texy set2))

node :: LaTeXC l => String -> String -> String -> l -> String -> l
node name pos options text edge =
  raw (fromString (printf "\\node[%s]%s(%s){" options pos' name)) <>
  text <>
  raw "}" <>
  raw (fromString edge) <>
  raw ";%\n"
  where
    pos' = case pos of
             "" -> ""
             _ -> "[right of=" ++ pos ++ "]"

explainFollows4 :: (LaTeXC l) => Symbol -> [Symbol] -> Symbol -> [Symbol] -> [Symbol] -> [Symbol] -> l
explainFollows4 lhs rhs1 nt rhs2 set1 set2 =
  center $
    raw "\\begin{tikzpicture}%\n" <>
    node lhs_ "" "" (texy lhs) (printf "edge [->, bend left=45] node[auto]{FOLLOW}(%s);%%\n" nt_) <>
    node to_ lhs_ "" (raw "$\\to$") "" <>
    mconcat (map (\(a,b,x) -> node b a "" (texy x) "") rhs1_) <>
    node nt_ (if null rhs1 then to_ else (snd3 (last rhs1_))) "rectangle,draw,fill=yellow!30" (texy nt) "" <>
    mconcat (map (\(a,b,x) -> node b a "" (texy x) "") rhs2_) <>
    raw "\\end{tikzpicture}%\n" <>
    par <>
    raw "$\\text{FOLLOW}(\\text{" <>
    texy nt <>
    raw "}) = \\{\\," <>
    set1' <>
    raw "\\,\\} \\cup \\{\\, \\text{" <>
    textcolor (DefColor Red) set2' <>
    raw "} \\,\\}$%%\n" <>
    raw "%\n"
  where
    lhs_ : names1 @ (to_ : names2) = map (:[]) ['a' ..]
    rhs1_ = zip3 names1 names2 rhs1
    names3@(nt_ : names4) = drop (length rhs1) names2
    rhs2_ = zip3 names3 names4 rhs2
    set1' = mconcat (intersperse (raw ",") (map texy set1))
    set2' = mconcat (intersperse (raw ",") (map texy set2))

buildTableLL1 :: View -> View
buildTableLL1 v@View{step = i, grammar, nts, nullable, first, follow, tableLL1, tableLL1Doc, expl} = v'
  where
    nterms = map info nts
    nullableSet = map (info . fst) (filter (info . last . snd) (zip nts nullable))
    firstSets = zip (map info nts) (map (map info) first)
    followSets = zip (map info nts) (map (map info) follow)
    expl0 = addExplanation expl i (center (textbf "Construção da tabela LL(1)"))
    ((i1, table1, expl1), grammar1) =
      mapAccumLFliped grammar (i+1, tableLL1, expl0) $ \(i, table, e) prod@(Doc a (Doc b lhs : rhs)) ->
        let (i', nl, fs, rhs', e') = firstOfSeq i nullableSet firstSets e lhs rhs
            (i'', flws) | nl = (i', fromMaybe [] (lookup lhs followSets))
                        | otherwise = (i', [])
            cleanProd = Doc [(Vis,B,i''),(Shade,BE,i'')] (lhs : map info rhs)
            table' = table ++ [((lhs,t), cleanProd) | t <- merge flws (map info fs)]
            e'' = addExplanation e' i' (explainLL1 lhs rhs1_ m_rhs2_ flws fSets)
            (rhs1_,m_rhs2_) = case span (flip elem nullableSet) (map info rhs) of
                                (rhs1__,x:rhs2__) -> (rhs1__ ++ [x], Just rhs2__)
                                (rhs1__,[]) -> (rhs1__,Nothing)
            fSets = map (\x -> fromMaybe [x] (lookup x firstSets)) rhs1_
            --e'' = e'
        in ((i''+1, table', e''), Doc a (Doc b lhs : rhs'))
    v' = v{step = i1, grammar = grammar1, tableLL1 = table1, tableLL1Doc = [(Vis,B,i)], expl = expl1}

firstOfSeq
  :: Slide
  -> [Symbol]
  -> [(Symbol, [Symbol])]
  -> [Doc LaTeX]
  -> Symbol
  -> [Doc Symbol]
  -> (Slide, Bool, [Doc Symbol], [Doc Symbol], [Doc LaTeX])
firstOfSeq slideNo nullableSet firstSets expl lhs symbolSeq =
  loopSymbolSeq slideNo expl [] [] symbolSeq
  where
    loopSymbolSeq i expl fSet previousSyms [] =
      (i, True, fSet, reverse previousSyms, expl)
    
    loopSymbolSeq i expl fSet previousSyms (sym@(Doc _ s@(T _)) : restSyms) =
      (i+1, False, fSet', syms', expl')
      where
        fSet' = mergeDoc fSet [Doc [(Vis,B,i),(Alert,BE,i)] s]
        previousSyms' = reverse previousSyms
        syms' = previousSyms' ++ addDoc [(Alert,BE,i)] sym : restSyms
        expl' = addExplanation expl i $
                  explanation previousSyms' s restSyms fSet [s]
                    
    loopSymbolSeq i expl fSet previousSyms (sym@(Doc _ s@(N _)) : restSyms)
      | elem s nullableSet =
          loopSymbolSeq (i+1) expl' fSet' previousSyms' restSyms
      | otherwise =
          (i+1, False, fSet', reverse previousSyms' ++ restSyms, expl')
      where
        Just fs = lookup s firstSets
        fSet' = mergeDoc fSet (map (Doc [(Vis,B,i),(Alert,BE,i)]) fs)
        previousSyms' = addDoc [(Alert,BE,i)] sym : previousSyms
        expl' = addExplanation expl i $
                  explanation (reverse previousSyms) s restSyms fSet fs

    explanation syms1 sym syms2 fSet1 fSet2 =
      center $ math $
        text "FIRST" <>
        "(" <>
        sepEndBy thinspace (map (textcolor (DefColor Cyan) . texy . info) syms1) <>
        textcolor (DefColor Red) (texy sym) <>
        sepBeginBy thinspace (map (texy . info) syms2) <>
        ") = " <>
        cup (set (sepBy thinspace (map (texy . info) fSet1)))
            (set (sepBy thinspace (map (textcolor (DefColor Red) . texy) fSet2)))

explainLL1 :: (LaTeXC l) => Symbol -> [Symbol] -> Maybe [Symbol] -> [Symbol] -> [[Symbol]] -> l
explainLL1 lhs rhs1 m_rhs2 flw_lhs fst_rhs1 =
  center $
       tabular Nothing (replicate (2+n1+n2) CenterColumn) (sepBy lnbk [r1,r2,r3])
    -- <> par
    -- <> "NEXT:" <> quad
    -- <> sepBy thinspace (map texy (foldr union flw_lhs fst_rhs1))
  where
    n1 = length rhs1
    n2 = maybe 0 length m_rhs2
    n_ = maybe 0 (const 1) m_rhs2
    r1 = foldl1 (&) (mempty : mempty : replicate (n1-n_) "nullable" ++ replicate (n2+n_) mempty)
    r2 = foldl1 (&) (texy lhs : math to : map texy (rhs1 ++ fromMaybe [] m_rhs2))
    r3 = foldl1 (&) (sepBy thinspace (map texy flw_lhs) :
                     mempty :
                     map (sepBy thinspace . map texy) fst_rhs1 ++
                     replicate n2 mempty)


merge :: Eq a =>[a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) ys =
  case break (==x) ys of
    (ys1, _ : ys2) -> x : merge xs (ys1 ++ ys2)
    _ -> x : merge xs ys

mergeDoc :: Eq a => [Doc a] -> [Doc a] -> [Doc a]
mergeDoc [] ys = ys
mergeDoc xs [] = xs
mergeDoc (x@(Doc a p):xs) ys =
  case break (\(Doc _ q) -> p == q) ys of
    (ys1, Doc b _ : ys2) -> Doc (a ++ b) p : mergeDoc xs (ys1 ++ ys2)
    _ -> x : mergeDoc xs ys

lookupIndex :: Eq a1 => [a1] -> [a] -> a1 -> Maybe a
lookupIndex [] _ _ = Nothing
lookupIndex (index:indexes) (x:xs) key | key == index = Just x
                                       | otherwise = lookupIndex indexes xs key

isNullable :: Eq a => [a] -> [[Doc Bool]] -> a -> Bool
isNullable nterms lst nterm = case lookupIndex nterms lst nterm of
                               Just ks@(_:_) -> info (last ks)
                               _ -> False

whileChange :: (t -> t -> Bool) -> t -> (t -> t) -> t
whileChange test x f
  | test x y = y
  | otherwise = whileChange test y f
  where
    y = f x

lookup2 :: Eq a => a -> [a] -> [a1] -> Maybe ([a1], a1, [a1])
lookup2 x lst1 lst2 = lookup2' lst1 lst2 []
  where
    lookup2' [] [] _ = Nothing
    lookup2' (y:ys) (z:zs) acc | x == y = Just (reverse acc, z, zs)
                               | otherwise = lookup2' ys zs (z:acc)

splitByNt :: Eq a => a -> [a] -> [t] -> ([t], [t])
splitByNt _ [] _ = ([], [])
splitByNt x (y:ys) zs'@(z:zs) | x == y = ([], zs')
                              | otherwise = let (zs1,zs2) = splitByNt x ys zs
                                            in (zs1 ++ [z], zs2)


texyView :: (Monad m) => View -> LaTeXT_ m
-- texyView v = do
--   tabular (Just Top) [Separator (raw ""), LeftColumn, Separator (raw "")] $ do
--     tabular (Just Top) [Separator (raw ""), LeftColumn, Separator (raw ""), RightColumn, Separator (raw "")] $
--       texyViewGrammar v & texyViewNFF v
--     lnbk
--     texyViewTableLL1 v
--   vfill
--   texyViewExplain v
texyView v = do
  texyViewGrammar v
  hfill
  texyViewNFF v
  vfill
  texyViewTableLL1 v
  vfill
  texyViewExplain v

texyViewGrammar :: (Monad m) => View -> LaTeXT_ m
texyViewGrammar v@View{grammar} = do
  let names = map (\(Doc _ (Doc _ lhs : _)) -> case lhs of N x -> x; T x -> x) grammar
  raw "\\newdimen\\largest"
  raw "\\widest("
  sequence_ (intersperse "," (map fromString names))
  raw ")\\largest%\n"
  tabular (Just Top) [Separator (raw ""), LeftColumn, Separator (raw "")] $ do
    sequence_ $ intersperse lnbk $ flip map grammar $ \(Doc a (lhs : rhs)) ->
      applyEffects a $ do
        makebox (Just (CustomMeasure (raw "\\largest"))) (Just HLeft) (texy lhs)
        hspace (Pt 1) <> math to <> hspace (Pt 1)
        sequence_ (intersperse (hspace (Pt 2)) (map texy rhs))

texyViewNFF :: (Monad m) => View -> LaTeXT_ m
texyViewNFF v@View{nts, nullable, first, follow, tableDoc, nullableDoc, firstDoc, followDoc} =
  applyEffects tableDoc $ do
    tabular
      (Just Top)
      [VerticalLine, LeftColumn, VerticalLine, LeftColumn, VerticalLine, LeftColumn, VerticalLine, LeftColumn, VerticalLine] $
      do raw "\\firsthline"
         textbf "NT" & textbf "Nullable" & textbf "First" & textbf "Follow"
         lnbk
         hline
         (forM_ (zip4 nts nullable first follow) $ \(nt, nullable, first, follow) ->
           do texy nt
               & sequence_
                   (intersperse
                     (raw "~")
                     (flip map nullable $ \(Doc a x) ->
                       applyEffects a (math (if x then checkmark else comm0 "times"))))
               & sequence_ (intersperse (hspace (Pt 4)) (map texy first))
               & sequence_ (intersperse (hspace (Pt 4)) (map texy follow))
              lnbk
              hline
          )

texyViewTableLL1 :: (Monad m) => View -> LaTeXT_ m
texyViewTableLL1 v@View{nts, nullable, first, follow, tableLL1, tableLL1Doc, ts, nonts} =
  applyEffects tableLL1Doc $ do
    tabular
      (Just Top)
      (VerticalLine : (intersperse VerticalLine (LeftColumn : map (const LeftColumn) ts)) ++ [VerticalLine]) $
      do hline
         foldl (&) (raw "") (map (textbf . texy) ts)
         lnbk
         hline
         forM_ nonts $ \nt -> do
           foldl1 (&) $
             texy nt :
               ( flip map ts $ \t ->
                   let lst1 = map
                              -- ((\(Doc a (lhs:rhs)) -> applyEffects a (foldr1 (<>) (texy lhs : math to : map texy rhs))) . snd)
                              ((\(Doc a (lhs:rhs)) ->
                                  applyEffects a
                                              (if null rhs
                                               then math epsilon
                                               else mconcat (intersperse (raw "~") (map texy rhs)))
                               ) . snd)
                              (filter (\((a,b),c) -> a == nt && b == t) tableLL1)
                       -- y = foldr (<>) (raw "") (intersperse (raw "\\newline ") lst1)
                   in tabular (Just Top) [Separator (raw ""), LeftColumn, Separator (raw "")] $ foldl (<>) (raw "") (intersperse lnbk lst1)
               )
           lnbk
           hline

texyViewExplain :: (Monad m) => View -> LaTeXT_ m
texyViewExplain View{expl} = do
  setbeamercovered [Invisible]
  overprint $ flushleft $ do
    forM_ expl $ \(Doc attrs text) ->
      applyEffects attrs (fromLaTeX text)

zip4 :: [t] -> [t1] -> [t2] -> [t3] -> [(t, t1, t2, t3)]
zip4 (a:as) (b:bs) (c:cs) (d:ds) = (a,b,c,d) : zip4 as bs cs ds
zip4 _ _ _ _ = []



-- breaks a list at every element that satisfies a test
-- example:
--    breakAll even [5,7,15,2,3,5,0,1,11,19,-100]
--      [ ([5,7,15],[2,3,5,0,1,11,19,-100])
--      , ([5,7,15,2,3,5],[0,1,11,19,-100])
--      , ([5,7,15,2,3,5,0,1,11,19],[-100])
--      ]

breakAll :: (a -> Bool) -> [a] -> [([a],[a])]
breakAll test list = breakAllLoop [] list
  where
    breakAllLoop _ [] = []
    breakAllLoop prefix suffix@(x:xs)
      | test x = (reverse prefix, suffix) : breakAllLoop (x:prefix) xs
      | otherwise = breakAllLoop (x:prefix) xs


latexNuFiFo :: Monad m => Maybe String -> Grammar -> LaTeXT_ m
latexNuFiFo fontScale grammar = do
  -- the document preamble
  documentclass [] beamer
  usepackage [utf8] inputenc
  usepackage [] amssymb
  usepackage [] tikz
  raw "\\usepackage[brazilian]{babel}%\n"
  raw "\\usepackage{etex}%\n"
  raw "\\usepackage{newtxtext}%\n"
  raw "\\usepackage[varg]{newtxmath}%\n"
  raw "\\usepackage{array}%\n"
  raw "\\usepackage{widest}%\n"
  raw "\\usepackage{relsize}%\n"
  raw "\\reserveinserts{28}%\n"
  raw "\\setlength{\\tabcolsep}{3pt}%\n"
  raw "\\renewcommand{\\familydefault}{\\sfdefault}%\n"
  raw "\\newcommand<>{\\bgyellow}[1]{{\\fboxsep 0pt\\only#2{\\colorbox{yellow!30}}{#1}}}%\n"
  raw "\\newcommand<>{\\bggreen}[1]{{\\fboxsep 0pt\\only#2{\\colorbox{green!30}}{#1}}}%\n"
  raw "\\newcommand<>{\\bg}[2]{{\\fboxsep 0pt\\only#3{\\colorbox{#1}}{#2}}}%\n"
  -- usetheme CambridgeUS
  -- raw "\\usetheme{OuroPreto}%\n"
  raw "\\setbeamersize{text margin left=0.1em, text margin right=0.1em}%\n"
  raw "\\setbeamertemplate{frametitle}{}%\n"
  setbeamercovered [Transparent Nothing]
  author "José Romildo Malaquias"
  title "Nullable, First and Follow"
  -- the document body
  document $ do
    frame maketitle
    -- raw "\\makeatletter % to change template\n\
    --     \\\setbeamertemplate{headline}[default] % not mandatory, but I though it was better to set it blank\n\
    --     \\\def\\beamer@entrycode{\\vspace*{-\\headheight}} % here is the part we are interested in :)\n\
    --     \\\makeatother%\n"
    frame $ do
      frametitle "Exemplo"
      normalsize
      -- small
      -- footnotesize
      -- scriptsize
      -- tiny
        $ do
        -- raw "\\smaller"
        maybe mempty (raw . T.pack . printf "\\relscale{%s}") fontScale
        texyView (build grammar)
