module Factorization where

import Data.List (sortOn, union, isPrefixOf)
import Grammar (Grammar(..), Production(..))
import Util (groupOn)
import Debug.Trace (trace, traceShowId)

--Factorize :: Grammar -> Grammar
factorize (Grammar rules) =
  map (factorout . fmap (map rhs)) (groupOn lhs rules)

factorout (left, rights) =
  (left, rights')
  where
    divs = map divisions rights
    prefixes = reverse $ sortOn length $ filter (not . null) $ foldl union [] $ map (map fst) $ divs
    rights' = filter ((>1) . length) [ r | p <- prefixes, r <- rights, isPrefixOf p r ]

                                                                                    
-- factorout (left, rights) =
--   traceShowId prefixes
--   -- traceShowId (map (fmap (reverse . divisions . map rhs)) groupsOfRules)
--   where
--     groupsOfRules = map (fmap (map rhs)) (groupOn lhs rules)
--     divs = map (fmap (map divisions)) groupsOfRules
--     prefixes = map (fmap (reverse . sortOn length . filter (not . null) . foldl union [] . map (map fst))) divs
--     -- commonPrefixes = map (fmap 


divisions :: [a] -> [([a], [a])]
divisions [] = []
divisions [x] = [([], [x]), ([x], [])]
divisions l@(x:xs) = ([], l) : [ (x : y, ys) | (y, ys) <- divisions xs ]
                   
