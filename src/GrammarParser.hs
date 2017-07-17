{-# LANGUAGE FlexibleContexts #-}

module GrammarParser where

import Data.Char (isSpace)
import Text.Parsec

import Grammar

grammarParser :: Stream s m Char => ParsecT s u m Grammar
grammarParser =
  do spaces
     prules <- many prule
     eof
     return (Grammar prules)

prule :: Stream s m Char => ParsecT s u m Production
prule =
  do nt <- lexeme nonterminal
     lexeme (string "->")
     right <- many (lexeme (terminal <|> nonterminal))
     spaces
     return (nt ::= right)

nonterminal :: Stream s m Char => ParsecT s u m Symbol
nonterminal =
  do symbol <- many1 (letter <|>
                      digit <|>
                      char '_' <|>
                      char '\'')
     return (N symbol)


terminal :: Stream s m Char => ParsecT s u m Symbol
terminal =
  do char '"'
     symbol <- many1 (    satisfy (\x -> x /= '"' && x /= '\\' && x /= '\n')
                      <|> (char '\\' >> (    (char '"' >> return '"')
                                         <|> (char '\\' >> return '\\'))))
     char '"'
     return (T symbol)

lexeme :: Stream s m Char => ParsecT s u m b -> ParsecT s u m b
lexeme p =
  do x <- p
     skipMany (satisfy (\c -> isSpace c && c /= '\n'))
     return x
