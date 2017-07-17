{-# LANGUAGE OverloadedStrings #-}

module LR where

import Control.Monad.Identity
import Control.Monad.Writer
-- import Data.Tree
import Data.List (intersperse)
import Text.LaTeX
import Text.LaTeX.Base.Syntax (LaTeX(TeXRaw,TeXEnv), TeXArg(FixArg,OptArg))
import Text.LaTeX.Base.Class (LaTeXC, fromLaTeX, liftL)
import Text.LaTeX.Packages.Trees.Forest (pforest)
-- import Text.LaTeX.Packages.Trees.Qtree
-- import Text.LaTeX.Packages.TikZ (tikz)
import Text.LaTeX.Packages.Inputenc
import Text.LaTeX.Packages.Color (textcolor, ColSpec(DefColor), Color(Blue))

import Grammar

type Input = [Symbol]

type State = Int

data Action
  = Shift State
  | Reduce Int
  | Goto State
  | Accept
  | Error
  deriving (Show)

type LRTable = [((State,Symbol),Action)]

type LRStack = [(State,Tree Symbol)]

data Act
  = S Int
  | R Int [Symbol] State
  | A
  | E
  deriving (Show)

instance Texy Action where
  texy (Shift s)  = fromString ("s" ++ show s)
  texy (Reduce r) = fromString ("r" ++ show r)
  texy (Goto s)   = fromString ("g" ++ show s)
  texy Accept     = fromString "a"
  texy Error      = fromString "e"

instance Texy Act where
  texy (S s)      = fromString ("s" ++ show s)
  texy (R r xs s) = fromString ("r" ++ show r ++ "g" ++ show s ++ " ") <> texyProduction xs
  texy A          = fromString "a"
  texy E          = fromString "e"

g_3_1 :: Grammar
g_3_1 =
  [ [N "P", N "S", T "$"]
  , [N "S", N "S", T ";", N "S"]
  , [N "S", T "id", T ":=", N "E"]
  , [N "S", T "print", T "(", N "L", T ")"]
  , [N "E", T "id"]
  , [N "E", T "num"]
  , [N "E", N "E", T "+", N "E"]
  , [N "E", T "(", N "S", T ",", N "E", T ")"]
  , [N "L", N "E"]
  , [N "L", N "L", T ",", N "E"]
  ]

table_g_3_1 :: LRTable
table_g_3_1 =
  [ ((1,  T "id"   ), Shift 4)
  , ((1,  T "print"), Shift 7)
  , ((1,  N "S"    ), Goto 2)
  , ((2,  T ";"    ), Shift 3)
  , ((2,  T "$"    ), Accept)
  , ((3,  T "id"   ), Shift 4)
  , ((3,  T "print"), Shift 7)
  , ((3,  N "S"    ), Goto 5)
  , ((4,  T ":="   ), Shift 6)
  , ((5,  T ";"    ), Reduce 1)
  , ((5,  T ","    ), Reduce 1)
  , ((5,  T "$"    ), Reduce 1)
  , ((6,  T "id"   ), Shift 20)
  , ((6,  T "num"  ), Shift 10)
  , ((6,  T "("    ), Shift 8)
  , ((6,  N "E"    ), Goto 11)
  , ((7,  T "("    ), Shift 9)
  , ((8,  T "id"   ), Shift 4)
  , ((8,  T "print"), Shift 7)
  , ((8,  N "S"    ), Goto 12)
  , ((9,  T "id"   ), Shift 20)
  , ((9,  T "num"  ), Shift 10)
  , ((9,  T "("    ), Shift 8)
  , ((9,  N "E"    ), Goto 15)
  , ((9,  N "E"    ), Goto 14)
  , ((10, T ";"    ), Reduce 5)
  , ((10, T ","    ), Reduce 5)
  , ((10, T "+"    ), Reduce 5)
  , ((10, T ")"    ), Reduce 5)
  , ((10, T "$"    ), Reduce 5)
  , ((11, T ";"    ), Reduce 2)
  , ((11, T ","    ), Reduce 2)
  , ((11, T "+"    ), Shift 16)
  , ((11, T "$"    ), Reduce 2)
  , ((12, T ";"    ), Shift 3)
  , ((12, T ","    ), Shift 18)
  , ((13, T ";"    ), Reduce 3)
  , ((13, T ","    ), Reduce 3)
  , ((13, T "$"    ), Reduce 3)
  , ((14, T ","    ), Shift 19)
  , ((14, T ")"    ), Shift 13)
  , ((15, T ","    ), Reduce 8)
  , ((15, T ")"    ), Reduce 8)
  , ((16, T "id"   ), Shift 20)
  , ((16, T "num"  ), Shift 10)
  , ((16, T "("    ), Shift 8)
  , ((16, N "E"    ), Goto 17)
  , ((17, T ";"    ), Reduce 6)
  , ((17, T ","    ), Reduce 6)
  , ((17, T "+"    ), Shift 16)
  , ((17, T ")"    ), Reduce 6)
  , ((17, T "$"    ), Reduce 6)
  , ((18, T "id"   ), Shift 20)
  , ((18, T "num"  ), Shift 10)
  , ((18, T "("    ), Shift 8)
  , ((18, N "E"    ), Goto 21)
  , ((19, T "id"   ), Shift 20)
  , ((19, T "num"  ), Shift 10)
  , ((19, T "("    ), Shift 8)
  , ((19, N "E"    ), Goto 23)
  , ((20, T ";"    ), Reduce 4)
  , ((20, T ","    ), Reduce 4)
  , ((20, T "+"    ), Reduce 4)
  , ((20, T ")"    ), Reduce 4)
  , ((20, T "$"    ), Reduce 4)
  , ((21, T ")"    ), Shift 22)
  , ((22, T ";"    ), Reduce 7)
  , ((22, T ","    ), Reduce 7)
  , ((22, T "+"    ), Reduce 7)
  , ((22, T ")"    ), Reduce 7)
  , ((22, T "$"    ), Reduce 7)
  , ((23, T ","    ), Reduce 9)
  , ((23, T "+"    ), Shift 16)
  , ((23, T ")"    ), Reduce 9)
  ]

test :: Input
test = map T ["id",":=","num",";"
             ,"id",":=","id","+","(","id",":=","num","+","num",",","id",")","$"
             ]

lr :: Grammar -> LRTable -> Input -> Writer [(LRStack, State, Input, Act)] Bool
lr grammar table =
  go [] 1
  where
    go stack state (input@(x:xs)) =
      case action of
        Error -> do tell [(stack,state,input,E)]
                    return False
        Accept -> do tell [(stack,state,input,A)]
                     return True
        Shift s -> do tell [(stack,state,input,S s)]
                      go ((state,Node (Just x) []):stack) s xs
        Reduce r -> let lhs:rhs = grammar!!r
                        (state',stack',nodes) = case rhs of
                                                  [] -> (state,stack,[])
                                                  _ -> let (as,bs) = splitAt (length rhs) stack
                                                       in (fst (last as),bs,map snd (reverse as))
                        Just (action'@(Goto state'')) = lookup (state',lhs) table
                    in do tell [(stack,state,input,R r (lhs:rhs) state'')]
                          go ((state',Node (Just lhs) nodes):stack') state'' input
      where
        action = case lookup (state,x) table of
                   Nothing -> Error
                   Just action -> action


showResult :: [(LRStack, State, Input, Act)] -> IO ()
showResult [] = return ()
showResult ((stack, state, input, actions):rest) =
  do forM_ (reverse stack) $ \(s,Node x _) -> putStr (show s ++ " " ++ show x ++ " ")
     putStr (show state ++ " ")
     putStr (show input ++ " ")
     putStrLn (show actions)
     showResult rest

docLR :: Monad m => [(LRStack, State, Input, Act)] -> LaTeXT_ m
docLR x =
  do documentclass [a4paper] article
     raw "\\usepackage[lmargin=1cm]{geometry}"
     raw "\\usepackage[]{longtable}"
     usepackage [utf8] inputenc
     usepackage [] pforest
     -- usepackage [] qtree
     -- usepackage [] "tikz-qtree"
     -- usepackage [] tikz
     author "José Romildo Malaquias"
     title "LR Parsing"
     document $
       do maketitle
          section "Parsing"
          center $ longtable Nothing [LeftColumn,LeftColumn] $ traceLR x

traceLR :: Monad m => [(LRStack, State, Input, Act)] -> LaTeXT_ m
traceLR [] = return ()
traceLR ((stack, state, input, actions):rest) =
  do (do forM_ (reverse stack) $ \(s, t@(Node (Just x) _)) ->
           do textcolor (DefColor Blue) $ texy s
              -- texy x
              tree texy t
         textcolor (DefColor Blue) $ textbf $ texy state
         hfill
         mconcat (intersperse " " (fmap texy input))
      &
      texy actions
      )
     lnbk
     hline
     -- rule Nothing (CustomMeasure linewidth) (Pt 1)
     -- newline
     traceLR rest

main :: IO ()
main =
  let (accepted,trace) = runWriter (lr g_3_1 table_g_3_1 test)
  in execLaTeXT (docLR trace) >>= renderFile "simple.tex"




longtable :: LaTeXC l =>
           Maybe Pos   -- ^ This optional parameter can be used to specify the vertical position of the table.
                       --   Defaulted to 'Center'.
        -> [TableSpec] -- ^ Table specification of columns and vertical lines.
        -> l       -- ^ Table content. See '&', 'lnbk', 'hline' and 'cline'.
        -> l       -- ^ Resulting table syntax.
longtable Nothing ts  = liftL $ TeXEnv "longtable" [ FixArg $ TeXRaw $ renderAppend ts ]
longtable (Just p) ts = liftL $ TeXEnv "longtable" [ OptArg $ TeXRaw $ render p , FixArg $ TeXRaw $ renderAppend ts ]
