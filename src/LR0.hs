{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module LR0 where

import           Control.Monad (forM_, foldM, foldM_, unless, when)
import           Control.Monad.State (get, put, execState)
import qualified Control.Monad.State as S (State)
import           Data.List (intercalate, intersperse, partition, find, findIndex, nub, sort, (\\), groupBy)
import           Debug.Trace (trace)

import           Text.LaTeX
-- import           Text.LaTeX.Base.Syntax (LaTeX(TeXRaw,TeXEnv), TeXArg(FixArg,OptArg))
-- import           Text.LaTeX.Base.Class (LaTeXC, fromLaTeX, liftL, fromLaTeX)
import           Text.LaTeX.Packages.Beamer
import           Text.LaTeX.Packages.AMSMath (math, to, bullet)
import           Text.LaTeX.Packages.AMSSymb (amssymb)
import           Text.LaTeX.Packages.Inputenc (inputenc, utf8)
import           Text.LaTeX.Packages.Relsize (prelsize, textsmaller, smaller)
import           Text.LaTeX.Packages.Color (textcolor, ColSpec(DefColor), Color(Blue))
import           Text.LaTeX.Packages.TikZ (tikz, tikzpicture)

import           Doc
import           Grammar
import           Util

for :: [a] -> (a -> b) -> [b]
for = flip map

groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' eq (x:xs) = (x:ys) : groupBy' eq zs
  where
    (ys,zs) = partition (eq x) xs

elem' :: (a -> Bool) -> [a] -> Bool
elem' _ [] = False
elem' test (x:xs) = test x || elem' test xs

type State = Int

type DocItem = Doc (Int, Doc Symbol, [Doc Symbol], [Doc Symbol])
type DocState = Doc (Doc State, [[DocItem]])
type DocTrans = Doc (State, (Symbol, State))
type DocProduction = [Doc Symbol]
type DocGrammar = [Doc (Doc Int, [Doc Symbol])]

data DocLR0 =
  DocLR0
  { slide     :: Slide
  , grammar   :: DocGrammar
  , sts       :: [DocState]
  , trans     :: [DocTrans]
  , nextState :: State
  }
  deriving (Show)


initialDocLR0 :: Grammar -> DocLR0
initialDocLR0 grm@(Grammar productions) =
  DocLR0
  { slide     = 1
  , grammar   = map (\(i, prod) -> Doc [] (Doc [] i, map (\sym -> Doc [] sym) prod)) productions''
  , sts       = []
  , trans     = []
  , nextState = 1
  }
  where
    Just (N start) = startSymbol grm
    newStart | elem (N "S") (nonterminals grm) = start ++ "'"
             | otherwise =  "S"
    productions' = map (\(lhs::=rhs) -> lhs:rhs) ((N newStart ::= [N start, T "$"]) : productions)
    productions'' = zip [0..] productions'

buildLR0 :: Grammar -> DocLR0
buildLR0 grm =
  execState buildView (initialDocLR0 grm)

buildView :: S.State DocLR0 ()
buildView = do
  buildViewGrammar
  buildViewAutomata

buildViewGrammar :: S.State DocLR0 ()
buildViewGrammar = do
  v@DocLR0{slide=i, grammar=g} <- get
  let g1 = for g $ addDoc [(Uncover,B,i)]
      i1 = i + 1
      Doc ov1 (pn1, lhs1 : rhs1a : rhs1rest) : Doc ov2 (pn2, lhs2 : rhs2rest) : rest = g1
      g2 =
        Doc (ov1 ++ [(Invis,E,i1),(Shade,BE,i2)]) (pn1, lhs1 : addDoc [(Alert,BE,i2)] rhs1a : rhs1rest) :
        Doc (ov2 ++ [(Shade,B,i1),(Shade,E,i2)]) (pn2, addDoc [(Alert,B,i1),(Alert,E,i2)] lhs2 : rhs2rest) :
        rest
      i2 = i1 + 1
      i3 = i2 + 1
      g3 =
        for g2 $ \(Doc a (Doc b pn, rule)) ->
          Doc a (Doc (b ++ [(Invis,E,i2),(Alert,BE,i3)]) pn, rule)
      i4 = i3 + 1
  put v{slide=i4, grammar=g3}

buildViewAutomata :: S.State DocLR0 ()
buildViewAutomata = do
  v@DocLR0{slide=i, grammar=g, sts=sts0, nextState=s1, trans} <- get
  let Doc _ (_, Doc _ lhs : _) : _ = g
      (items1, g1) = newItems i lhs g
      (i2, g2, itemsLst) = closure (i+1) g1 items1
      st1 = Doc [(Vis,B,i)] (Doc [(Vis,B,i2),(Alert,BE,i2)] s1, itemsLst)
      sts1 = sts0 ++ [st1]
      (i3, s3, g3, sts3, trans3) = allTransitions (i2+1) (s1+1) g2 sts1
  put v{slide=i3, grammar=g3, sts=sts3, nextState=s3, trans=trans ++ trans3}
  return ()

newItems :: Slide -> Symbol -> DocGrammar -> ([DocItem], DocGrammar)
newItems i nt grammar =
  mapAccumLFliped grammar [] $ \items prod@(Doc _ (Doc _ pn, Doc _ lhs : rhs)) ->
    if nt == lhs
    then let item = Doc [(Invis,E,i-1),(Shade,BE,i)] (pn, Doc [] lhs, [], map (Doc [] . info) rhs)
         in (items ++ [item], addDoc [(Shade,BE,i)] prod)
    else (items, prod)

closure_ :: Slide -> DocGrammar -> [DocItem] -> (Slide, DocGrammar, [[DocItem]])
closure_ i grammar new_items =
  closure' i grammar [new_items] new_items
  where
    closure' i grammar itemsLst new_items =
      let (i1, grammar1, itemsLst1, new_items1) = go i grammar itemsLst new_items
      in if null new_items1
         then (i1, grammar1, itemsLst1)
         else closure' i1 grammar1 (itemsLst1 ++ [new_items1]) new_items1
      where
        go i grammar itemsLst items =
          foldl f (i, grammar, itemsLst, []) items
          where
            f x@(i, grammar, itemsLst, items) it@(Doc a (pn, lhs, rhs1, rhs2)) =
              case rhs2 of
                rhs2a@(Doc _ s@(N _)) : rhs2rest ->
                  let (items1, grammar1) = newItems i s grammar
                      it1 = Doc a (pn, lhs, rhs1, addDoc [(Alert,BE,i)] rhs2a : rhs2rest)
                      (itemsLst1, items2) = mergeItems itemsLst (it1 : items1)
                  in (i+1, grammar1, itemsLst1, items2)
                _ ->
                  x

closure :: Slide -> DocGrammar -> [DocItem] -> (Slide, DocGrammar, [[DocItem]])
closure i grammar new_items =
  whileNotEmpty new_items (i, grammar, [new_items]) $ \k n_items ->
    foldlFlipped n_items (k, []) $
      \x@((i, grammar, itemsLst), future_items) it@(Doc a (pn, lhs, rhs1, rhs2)) ->
        case rhs2 of
          rhs2a@(Doc _ symbol@(N _)) : rhs2rest
            | null items2 -> ((i+1, grammar1, itemsLst1), future_items)
            | otherwise   -> ((i+1, grammar1, itemsLst1 ++ [items2]), future_items ++ items2)
            where
              it1 = Doc a (pn, lhs, rhs1, addDoc [(Alert,BE,i)] rhs2a : rhs2rest)
              (items1, grammar1) = newItems i symbol grammar
              (itemsLst1, items2) = mergeItems itemsLst (it1 : items1)
          _ ->
            x

mergeItems :: [[DocItem]] -> [DocItem] -> ([[DocItem]], [DocItem])
mergeItems itemsLst items =
  foldl f (itemsLst, []) items
  where
    f (itemsLst, new_items) it1@(Doc ovs1 (pn1, lhs1, rhsA1, rhsB1)) =
      case findItem pn1 (length rhsA1) itemsLst of
        Just (itemsLst1, its1, it, its2, itemsLst2) ->
          let Doc ovs2 (pn2, lhs2, rhsA2, rhsB2) = it
              it' = Doc (ovs1 ++ ovs2)
                        (pn1,
                         mergeEffectsDocs lhs1 lhs2,
                         zipWith mergeEffectsDocs rhsA1 rhsA2,
                         zipWith mergeEffectsDocs rhsB1 rhsB2
                        )
          in (itemsLst1 ++ [its1 ++ [it'] ++ its2] ++ itemsLst2, new_items)
        _ ->
          (itemsLst, new_items ++ [it1])

findItem ::
  Int -> Int -> [[DocItem]] -> Maybe ([[DocItem]], [DocItem], DocItem, [DocItem], [[DocItem]])
findItem _ _ [] = Nothing
findItem prodNum dotPos (items : rest) =
  case break (\(Doc _ (pn, _, rhs1, _)) -> pn == prodNum && length rhs1 == dotPos) items of
    (items1, item : items2) ->
      Just ([], items1, item, items2, rest)
    _ ->
      case findItem prodNum dotPos rest of
        Just (itemsLst1, items1, item, items2, itemsLst2) ->
          Just (items : itemsLst1, items1, item, items2, itemsLst2)
        Nothing ->
          Nothing

mergeEffectsDocs :: Eq a => Doc a -> Doc a -> Doc a
mergeEffectsDocs (Doc ovs1 x1) (Doc ovs2 x2)
  | x1 == x2 = Doc (ovs1 ++ ovs2) x1
  | otherwise = error "mergeEffectsDocs: should be equal!"


allTransitions i ns grammar sts =
  go (i, ns, grammar, sts, []) []
  where
    go x stsDone =
      case stNumbers \\ stsDone of
        st : _ -> go (transitions x st) (st : stsDone)
        [] -> x
      where
        (_, _, _, sts', _) = x
        stNumbers = map (info . fst . info) sts'

transitions
  :: (Slide, State, DocGrammar, [DocState], [DocTrans])
  -> State
  -> (Slide, State, DocGrammar, [DocState], [DocTrans])
transitions (i, ns, grammar, sts, ts) stnumber =
  foldl mkTrans (i, ns, grammar, sts, ts) syms
  where
    Just ind = findIndex (\(Doc _ (Doc _ n, _)) -> n == stnumber) sts
    Doc a (pn, itemsLst) = sts !! ind
    syms = nub (filter ((/= T "$")) (map (info . head) (filter (not . null) (map (fth4 . info) (concat itemsLst)))))

    mkTrans ::
      (Slide, State, DocGrammar, [DocState], [DocTrans]) ->
      Symbol ->
      (Slide, State, DocGrammar, [DocState], [DocTrans])
    mkTrans (i, ns, grammar, sts, ts) sym =
      case break (\(Doc _ (_, its : _)) -> sameItemSet its new_items) sts1 of
        (sts1A, Doc a (Doc b x, itss1) : sts1B) -> (i+1, ns, grammar, sts2, ts2)
          where
            (itss2, []) = mergeItems itss1 new_items
            st2 = Doc a (Doc (b ++ [(Shade,BE,i)]) x, itss2)
            sts2 = sts1A ++ [st2] ++ sts1B
            ts2 = ts ++ [Doc [(Vis,B,i),(Alert,BE,i)] (stnumber, (sym, x))]
        _ -> (i2+1, ns+1, grammar2, sts2, ts2)
          where
            (i2, grammar2, itemsLst2) = closure (i+1) grammar new_items
            st2 = Doc [(Vis,B,i)] (Doc [(Alert,BE,i2)] ns, itemsLst2)
            sts2 = sts1 ++ [st2]
            ts2 = ts ++ [Doc [(Vis,B,i2),(Alert,BE,i2)] (stnumber, (sym, ns))]
      where
        Doc a (pn, itemsLst) = sts !! ind

        (new_items, itemsLst') =
          mapAccumLFliped itemsLst [] $ \new_items items ->
            mapAccumLFliped items new_items $ \new_items it@(Doc a (pn, lhs, rhsA, rhsB)) ->
              case rhsB of
                Doc b s : ss | s == sym -> (its1 ++ its2, old_item)
                  where
                    new_item = Doc [(Vis,B,i)] (pn,
                                                clearDoc lhs,
                                                Doc [(Alert,BE,i)] s : map clearDoc rhsA,
                                                map clearDoc ss
                                               )
                    old_item = Doc a (pn, lhs, rhsA, Doc (b ++ [(Alert,BE,i)]) s : ss)
                    ([its1], its2) = mergeItems [new_items] [new_item]
                _ ->
                  (new_items, it)

        sts1 = listSet sts ind (Doc a (pn, itemsLst'))


listSet (_ : xs) 0 y         = y : xs
listSet (x : xs) n y | n > 0 = x : listSet xs (n-1) y

sameItemSet =
  equalf (sort . map (\(Doc _ (pn1, _, rhsA1, _)) -> (pn1, length rhsA1)))
  

texyDocLR0 :: Monad m => DocLR0 -> LaTeXT_ m
texyDocLR0 doc = do
  tabular
    (Just Top)
    [Separator (raw ""), LeftColumn, Separator (raw ""), LeftColumn, Separator (raw ""), LeftColumn, Separator (raw "")] $ do
      texyDocGrammar (grammar doc) &
        texyDocAutomata (sts doc) (trans doc) &
        --texyDocAutomataGraphvix (sts doc) (trans doc) &
        raw "LR(0) Table"

texyDocGrammar :: Monad m => DocGrammar -> LaTeXT_ m
texyDocGrammar prods = do
  widest "\\largestnumber" (map (\(Doc _ (Doc _ i, _)) -> show i) prods)
  widest "\\largestlhs" (map (\(Doc _ (_, Doc _ lhs : _)) -> symbolName lhs) prods)
  tabular (Just Top) [Separator mempty, LeftColumn, Separator mempty ] $ do
    sequence_ $ intersperse lnbk $ flip map prods $ \(Doc a (number, lhs : rhs)) ->
      applyEffects a $ do
        makebox (Just (CustomMeasure (raw "\\largestnumber"))) (Just HLeft) (textsmaller (Just 2) $ texy number)
        hspace (Pt 2)
        makebox (Just (CustomMeasure (raw "\\largestlhs"))) (Just HRight) (texy lhs)
        -- hspace (Pt 1) <> 
        math to -- <> hspace (Pt 1)
        sequence_ (intersperse (hspace (Pt 2)) (map texy rhs))

texyDocAutomata :: Monad m => [DocState] -> [DocTrans] -> LaTeXT_ m
texyDocAutomata states transitions =
  do raw "\n"
     forM_ names_states $ \(name, Doc a (n, itemsLst)) -> do
       raw "\\setbox\\" >> fromString name >> raw "\\hbox{%\n"
       applyEffects a $ do
         tabular (Just Top) [VerticalLine, LeftColumn, VerticalLine] $ do
           multicolumn 1 [LeftColumn] (texy n)
           lnbk
           hline
           sequence_ $ intersperse (lnbk <> hline) $ for itemsLst $ \items ->
             sequence_ $ intersperse lnbk $ for items $ \(Doc b (_, lhs, rhs1, rhs2)) ->
               applyEffects b $ do
                 texy lhs
                 math to
                 sequence_ $ intersperse (raw "~") $ map texy (reverse rhs1)
                 textcolor (DefColor Blue) $ math $ bullet (raw "\\,") (raw "\\,")
                 sequence_ $ intersperse (raw "~") $ map texy rhs2
           lnbk
           hline
       raw "}%\n"
     raw "\\begin{tikzpicture}[baseline=(current bounding box.north),rounded corners]\n"
     raw "%\\graph [spring electrical Walshaw 2000 layout, node distance=2cm] {\n"
     raw "%\\graph [simple necklace layout] {\n"
     raw "\\graph [layered layout,\n\
         \         grow'=right,\n\
         \         grow down sep=2em,\n\
         \        ]{\n"
     groupTransitions names_states transitions
     raw "};\n"
     raw "\\end{tikzpicture}\n"
  where
    names_states = zip names states

names :: [String]
names = [ [x] | x <- ['a'..'z'] ] ++ [ x ++ [y] | x <- names, y <- ['a' .. 'z']]

texyst :: Monad m => [State] -> [(String, DocState)] -> Int -> LaTeXT_ m
texyst shownStates names_states stateNumber =
  do fromString name
     unless (elem stateNumber shownStates) $
       raw "/[as=\\copy\\" >> fromString name >> raw "]"
  where
    Just (name, _) = find (\(_, Doc _ (Doc _ n, _)) -> n == stateNumber) names_states

groupTransitions :: Monad m => [(String, DocState)] -> [DocTrans] -> LaTeXT_ m
groupTransitions names_states transitions = do
  foldM_
    ( \shown (Doc a (st1, (sym, st2))) -> do
         applyEffects [] $ do
           texyst shown names_states st1
           raw " -> "
           raw "[\"{"
           texy sym
           raw "}\""
           when (st1 == st2) (raw ",loop left,distance=2mm")
           raw "] "
           texyst shown names_states st2
           raw ";\n"
         return (st1 : st2 : shown)
    )
    []
    transitions                     

groupTransitions_ :: Monad m => [(String, DocState)] -> [DocTrans] -> LaTeXT_ m
groupTransitions_ names_states transitions0 = do
  go 1 Nothing []
  return ()
  where
    transitions = transitions0
    go :: Monad m => State -> Maybe Symbol -> [State] -> LaTeXT m [State]
    go src mSym done
      | elem src done =
        do fromString name
           maybe (raw "") (\sym -> raw "[>\"" >> texy sym >> raw "\"]") mSym
           return done
      | otherwise =
        do fromString name
           raw "/[as=\\copy\\"
           fromString name
           maybe (raw "") (\sym -> raw ",>\"" >> texy sym >> raw "\"") mSym
           raw "]"
           if null (filter ((== src) . fst . info) transitions)
             then do raw ";\n"
                     return done
             else do raw " -> { "
                     done' <-
                       foldM
                         (\done1 (Doc _ (_, (symbol, dest))) -> go dest (Just symbol) done1)
                         (insert src done)
                         (filter ((== src) . fst . info) transitions)
                     raw "};\n"
                     return done'
      where
        Just (name, _) = find (\(_, Doc _ (Doc _ n, _)) -> n == src) names_states
                     

texyDocAutomataGraphvix :: Monad m => [DocState] -> [DocTrans] -> LaTeXT_ m
texyDocAutomataGraphvix states transitions = do
  raw "\\tikzstyle{n}=[shape=rectangle,minimum size=1pt,inner sep=0pt,outer sep=0pt]\n"
  raw "\\begin{dot2tex}[fdp,autosize,straightedges,tikzedgelabels,styleonly,graphstyle={node distance=1mm,baseline=(current bounding box.north)}]\n"
  raw "digraph automata {\n"
  raw "mindist = 0.01;\n"
  raw "node [shape=\"none\"];\n"
  raw "node [style=\"n\"];\n"
  raw "edge [lblstyle=\"auto\",style=\"-stealth\"];\n"
  forM_ states $ \(Doc a (number, itemsLst)) -> do
    raw "s" >> texy (info number)
    raw " [texlbl=\"{{%\n"
    applyEffects a $ do
      tabular (Just Top) [VerticalLine, LeftColumn, VerticalLine] $ do
        multicolumn 1 [LeftColumn] (texy number)
        lnbk
        hline
        sequence_ $ intersperse (lnbk <> hline) $ for itemsLst $ \items ->
          sequence_ $ intersperse lnbk $ for items $ \(Doc b (_, lhs, rhs1, rhs2)) ->
            applyEffects b $ do
              texy lhs
              math to
              sequence_ $ intersperse (raw "~") $ map texy (reverse rhs1)
              textcolor (DefColor Blue) $ math $ bullet (raw "\\,") (raw "\\,")
              sequence_ $ intersperse (raw "~") $ map texy rhs2
        lnbk
        hline
    raw "}}\"];\n"
  forM_ transitions $ \(Doc a (s1, (sym, s2))) -> do
    raw "s" >> texy s1
    raw " -> "
    raw "s" >> texy s2
    raw " [texlbl=\""
    applyEffects a (texy sym)
    raw "\"];\n"
  -- raw "rankdir=LR;\n"
  raw "}\n"
  raw "\\end{dot2tex}\n"
  return ()
  
widest :: Monad m => String -> [String] -> LaTeXT_ m
widest dimenName textList = do
  raw "\\newdimen"
  raw (fromString dimenName)
  raw "\\widest("
  raw (fromString (intercalate "," textList))
  raw ")"
  raw (fromString dimenName)

latexLR0 :: Monad m => Grammar -> LaTeXT_ m
latexLR0 grammar = do
  -- the document preamble
  documentclass [a4paper] beamer
  raw "\\usepackage{ifluatex}\n"
  raw "\\ifluatex\n"
  raw "\\usepackage{polyglossia}\\setdefaultlanguage[]{brazil}\n"
  raw "\\usepackage{lmodern}\n"
  raw "\\else\n"
  usepackage [utf8] inputenc
  raw "\\usepackage[brazilian]{babel}\n"
  raw "\\usepackage{newtxtext}\n"
  raw "\\usepackage[varg]{newtxmath}\n"
  raw "\\fi\n"
  usepackage [] tikz
  usepackage [] amssymb
  usepackage [] prelsize
  raw "\\usepackage{etex}\n"
  raw "%\\usepackage{shellesc}\n"
  raw "\\usepackage{widest}\n"
  raw "\\usepackage[tikz,debug]{dot2texi}\n" -- autosize
  raw "\\usetikzlibrary{shapes,arrows}\n"
  raw "\\usetikzlibrary{quotes,graphs}\n"
  raw "\\ifluatex\n\\usetikzlibrary{graphdrawing}\n\\usegdlibrary{layered,force,circular}\n\\else\n\\fi\n"
  raw "%\\reserveinserts{28}\n"
  raw "\\newcommand<>{\\bgyellow}[1]{{\\fboxsep 0pt\\only#2{\\colorbox{yellow!30}}{#1}}}\n"
  -- usetheme CambridgeUS
  raw "\\usetheme{OuroPreto}\n"
  raw "\\setbeamersize{text margin left=0.2em, text margin right=0.2em}\n"
  setbeamercovered [Transparent Nothing]
  author "José Romildo Malaquias"
  title "Análise Sintática LR"
  -- the document body
  document $ do
    raw "\\setlength{\\tabcolsep}{1pt}\n"
    raw "\\scriptsize\n"
    smaller (Just 4)
    -- create boxes
    forM_ (take (length (sts doc)) names) $ \name ->
       raw "\\newbox\\" >> fromString name >> raw "\n"
    frame maketitle
    frame $ do
      raw "[fragile,plain]\n"
      -- frametitle "Construção da tabela LR(0)"
      texyDocLR0 doc
      raw "\n"
    raw "\n"
 where
   doc = buildLR0 grammar
