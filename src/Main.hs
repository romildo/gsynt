{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (when, (>=>))
import Data.List (find)
import System.Console.GetOpt (OptDescr(Option), ArgDescr(NoArg, ReqArg), ArgOrder(Permute), usageInfo, getOpt)
import System.Environment (getProgName, getArgs)
import System.IO (IOMode(..), withFile, hGetContents)
import Text.LaTeX (execLaTeXT, renderFile)
import Text.LaTeX.Base.Pretty (prettyLaTeX)
import Text.Parsec (parse, SourceName)

import Grammar
import GrammarParser
import LR0 (latexLR0)
import LR1 (lr1, ppLR1Table)
import SlidesNuFiFo (latexNuFiFo)



data Flag
  = Nullable
  | FIRST
  | FOLLOW
  | LL1
  | LR0
  | LR1
  | SLIDESNFF
  | LATEX
  | FontScale { fontScale :: String }
  deriving (Eq,Show)

options :: [OptDescr Flag]
options =
  [ Option ['n'] ["nullables"] (NoArg Nullable)        "print the nullables table"
  , Option ['f'] ["first"    ] (NoArg FIRST)           "print the first table"
  , Option ['w'] ["follow"   ] (NoArg FOLLOW)          "print the follow table"
  , Option ['l'] ["ll1"      ] (NoArg LL1)             "print the LL(1) parse table"
  , Option ['0'] ["lr0"      ] (NoArg LR0)             "print the LR(0) parse table"
  , Option ['1'] ["lr1"      ] (NoArg LR1)             "print the LR(1) parse table"
  , Option ['s'] ["slidesnff"] (NoArg SLIDESNFF)       "slides for nullable, first and follow"
  , Option ['x'] ["latex"    ] (NoArg LATEX)           "print the latex document"
  , Option ['z'] ["fontscale" ] (ReqArg FontScale "NUMBER") "main latex document font scale"
  ]

usedOpts :: String -> [String] -> IO ([Flag], Maybe String)
usedOpts prog args =
  case getOpt Permute options args of
    (o,[f],[]) -> return (o,Just f)
    (o,[ ],[]) -> return (o,Nothing)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Usage: " ++ prog ++ " [OPTION...] [FILE]"

process :: [Flag] -> SourceName -> String -> IO ()
process o f input =
  case (parse grammarParser f input) of
    Left err -> print err
    Right g  -> do putStrLn ">>>>>>>>>> Grammar: "
                   putStrLn (show g)
                   putStrLn ""
                   let nullables = nullableSet g
                       firsts = firstSets g nullables
                       follows = followSets g nullables firsts
                   when (elem Nullable o) $
                     do putStrLn ">>>>>>>>>> Nullable set: "
                        putStrLn (show nullables)
                        putStrLn ""
                   when (elem FIRST o) $
                     do putStrLn ">>>>>>>>>> First sets: "
                        putStrLn (showAssocList firsts)
                        putStrLn ""
                   when (elem FOLLOW o) $
                     do putStrLn ">>>>>>>>>> Follow sets: "
                        putStrLn (showAssocList follows)
                        putStrLn ""
                   when (elem LL1 o) $
                     do putStrLn ">>>>>>>>>> LL(1) table: "
                        putStrLn (showTable (ll1Table g))
                        putStrLn ""
                   when (elem LR1 o) $
                     do putStrLn ">>>>>>>>>> LR(1) table: "
                        putStrLn (ppLR1Table (lr1 g))
                        putStrLn ""
                   when (elem SLIDESNFF o) $
                     do putStrLn ">>>>>>>>>> Nullable, first and follow Latex document: "
                        let scale = fmap fontScale (find (\case {FontScale _ -> True; _ -> False}) o)
                        ltx <- execLaTeXT (latexNuFiFo scale g)
                        --renderFile (f ++ ".nufifo.tex") ltx
                        writeFile (f ++ ".nufifo.tex") (prettyLaTeX ltx)
                        putStrLn (f ++ ".nufifo.tex")
                        putStrLn ""
                   when (elem LR0 o) $
                     do putStrLn ">>>>>>>>>> LR(0) latex document: "
                        execLaTeXT (latexLR0 g) >>= renderFile (f ++ ".lr0.tex")
                        putStrLn (f ++ ".lr0.tex")
                        putStrLn ""
                     -- do putStrLn ">>>>>>>>>> LR(1) table: "
                     --    putStrLn (show (runLR0 g))
                     --    putStrLn ""

main :: IO ()
main =
  do putStrLn "Compiler Construction Tools"
     putStrLn "Version 0.1"
     putStrLn ""
     args <- getArgs
     prog <- getProgName
     (opts,mf) <- usedOpts prog args
     case mf of
       Just f -> withFile f ReadMode (hGetContents >=> process opts f)
       Nothing -> getContents >>= process opts "stdin"
