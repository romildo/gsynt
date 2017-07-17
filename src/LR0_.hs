{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module LR0 where

-- import           Control.Monad (forM, forM_)
import           Control.Monad.State (get, put, modify, runState)
import qualified Control.Monad.State as S (State)
import           Data.List (intercalate, partition)

import           Text.LaTeX
-- import           Text.LaTeX.Base.Syntax (LaTeX(TeXRaw,TeXEnv), TeXArg(FixArg,OptArg))
-- import           Text.LaTeX.Base.Class (LaTeXC, fromLaTeX, liftL, fromLaTeX)
import           Text.LaTeX.Packages.Beamer
import           Text.LaTeX.Packages.AMSMath (align_)
import           Text.LaTeX.Packages.Trees.Forest (pforest)
-- import Text.LaTeX.Packages.Trees.Qtree
-- import Text.LaTeX.Packages.TikZ (tikz)
import           Text.LaTeX.Packages.Inputenc
-- import           Text.LaTeX.Packages.Color (textcolor, ColSpec(DefColor), Color(Blue))

import           Grammar


groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' _ [] = []
groupBy' eq (x:xs) = (x:ys) : groupBy' eq zs
  where
    (ys,zs) = partition (eq x) xs

elem' :: (a -> Bool) -> [a] -> Bool
elem' _ [] = False
elem' test (x:xs) = test x || elem' test xs


type Slide = Int

type Action = String

data Doc a = Doc { info :: a
                 , overlays :: [(Slide,Action)]
                 }
           deriving (Show)

type DocItem = Doc (Doc Symbol,[Doc Symbol],[Doc Symbol])
type DocState = Doc ([DocItem],[DocItem])
type DocTrans = Doc [(State,Symbol,State)]
type DocGramar = Doc [Doc [Doc Symbol]]

data DocLR0 =
  DocLR0
  { slide   :: Slide
  , grammar :: DocGramar
  , st      :: DocState
  , trans   :: DocTrans
  }
  deriving (Show)

nextSlide :: S.State DocLR0 Slide
nextSlide =
  do doc@DocLR0{slide} <- get
     let newSlide = slide + 1
     put doc{slide = newSlide}
     return newSlide

actProduction :: Int -> Action -> S.State DocLR0 ()
actProduction i action =
  modify $ \doc@DocLR0{slide,grammar=Doc g govs} ->
    let (ps1,Doc p povs:ps2) = splitAt i g
        g' = ps1 ++ (Doc p ((slide,action):povs):ps2)
    in doc{grammar=Doc g' govs}

actProductionSymbol :: Int -> Int -> Action -> S.State DocLR0 ()
actProductionSymbol i j action =
  modify $ \doc@DocLR0{slide,grammar=Doc g govs} ->
    let (ps1, Doc p povs : ps2) = splitAt i g
        (ss1, Doc s sovs : ss2) = splitAt j p
        p' = ss1 ++ Doc s ((slide,action):sovs) : ss2
        g' = ps1 ++ Doc p' povs : ps2
    in doc{grammar=Doc g' govs}


type Item = (Symbol,[Symbol],[Symbol])

type State = (Int,[Item])


showItem :: Item -> String
showItem (nt,xs,ys) =
  show nt ++ " -> " ++
  intercalate " " (map show (reverse xs)) ++ " . " ++
  intercalate " " (map show ys)

showState :: State -> String
showState (counter,items) =
  unlines (show counter : map showItem items)

showLR0 :: [State] -> String
showLR0 states =
  unlines (map showState states)


startSymbol :: Grammar -> S.State DocLR0 Symbol
startSymbol (Grammar ((lhs:_):_)) = return lhs


initialDocLR0 :: Grammar -> DocLR0
initialDocLR0 (Grammar g) =
  DocLR0
  { slide   = 0
  , grammar = Doc (map (\prod -> Doc (map (\sym -> Doc sym []) prod) []) g) []
  , st      = Doc ([],[]) []
  , trans   = Doc [] []
  }

docLRLatex :: Monad m => DocLR0 -> LaTeXT_ m
docLRLatex x =
  do documentclass [a4paper] beamer
     -- raw "\\usepackage[lmargin=1cm]{geometry}"
     -- raw "\\usepackage[]{longtable}"
     usepackage [utf8] inputenc
     usepackage [] pforest
     -- usepackage [] qtree
     -- usepackage [] "tikz-qtree"
     -- usepackage [] tikz
     author "José Romildo Malaquias"
     title "LR Parsing"
     usetheme CambridgeUS
     document $
       do frame maketitle
          frame $
            do frametitle "Construção da tabela LR(0)"
               traceGrammar (grammar x)

traceGrammar :: Monad m => DocGramar -> LaTeXT_ m
traceGrammar (Doc prods _) =
  align_ $ map (\(Doc (Doc nt _ : _) _) -> texy nt) prods

buildLR0 :: Grammar -> S.State DocLR0 ()
buildLR0 (Grammar _g) =
  do actProduction 0 "invisible"
     -- actProductionNumbers "invisible"
     nextSlide
     actProductionSymbol 1 0 "alert"
     nextSlide
     actProduction 0 "visible"
     nextSlide
     return ()

runLR0 :: Grammar -> DocLR0
runLR0 g =
  snd (runState (buildLR0 g) (initialDocLR0 g))


-- selectProductions :: Grammar -> Symbol -> [[Symbol]]
-- selectProductions g lhs = filter ((== lhs) . head) g

-- mkItems :: Grammar -> Symbol -> [Item]
-- mkItems g nt =
--   map (\(lhs:rhs) -> (lhs,[],rhs)) (selectProductions g nt)

-- closure :: Grammar -> [Item] -> [Item]
-- closure g items =
--   go [] items
--   where
--     go items [] = reverse items
--     go items (i@(_,_,N x:_):is) =
--       go (i:items)
--          (is ++ filter (\i -> not (elem i items) && not (elem i is))
--                        (mkItems g (N x)))
--     go items (i:is) = go (i:items) is

-- push :: Item -> Item
-- push (lhs,stack,x:xs) = (lhs,x:stack,xs)

-- lr0 :: Grammar -> [State]
-- lr0 g =
--   go 2 [] [(1,closure g (mkItems g (startSymbol g)))]
--   where
--     go _ states [] = reverse states
--     go counter states (st@(_,s0):ss) =
--       go counter' (st:states) (ss++ss6)
--       where
--         s1 = filter (\(_,_,ys) -> not (null ys)) s0
--         ss2 = groupBy' (\(_,_,x:_) (_,_,y:_) -> x == y) s1
--         ss3 = map (map push) ss2
--         ss4 = map (closure g) ss3
--         ss5 = filter (\x -> isNew x st && all (isNew x) states && all (isNew x) ss) ss4
--         ss6 = zip [counter ..] ss5
--         counter' = counter + length ss5
--         isNew x (_,y) = x /= y

