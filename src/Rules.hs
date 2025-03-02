{-
-- EPITECH PROJECT, 2025
-- wolfram
-- File description:
-- Rules
-}
module Rules (applyRule, applyRuleG) where

import Data.Bits (testBit)

applyRule :: Int -> Int -> String -> Char -> String
applyRule rule 0 line c = line
applyRule rule start line c = 
  let newLine = line
      result = iterate (\l -> applyRuleG rule l c) newLine
  in result !! start

applyRuleG :: Int -> String -> Char -> String
applyRuleG rule line c = applyRuleGHelper rule (" " ++ line ++ " ") "" c

applyRuleGHelper :: Int -> String -> String -> Char -> String
applyRuleGHelper rule (x:y:z:rest) acc c =
  applyRuleGHelper rule (y:z:rest) (acc ++ [applyRuleToNeigh rule x y z c]) c
applyRuleGHelper _ [_,_] acc _ = acc
applyRuleGHelper _ [_] acc _ = acc
applyRuleGHelper _ [] acc _ = acc

applyRuleToNeigh :: Int -> Char -> Char -> Char -> Char -> Char
applyRuleToNeigh rule a b c mychar =
  let ruleTable = generateRuleTable rule mychar
  in case lookup (a, b, c) ruleTable of
       Just result -> result
       Nothing -> ' '

generateRuleTable :: Int -> Char -> [((Char, Char, Char), Char)]
generateRuleTable rule mychar =
  [ ((a, b, c), if testBit rule (7 - idx) then mychar else ' ')
  | (idx, (a, b, c)) <- zip [0..]
    [ (mychar, mychar, mychar)
    , (mychar, mychar, ' ')
    , (mychar, ' ', mychar)
    , (mychar, ' ', ' ')
    , (' ', mychar, mychar)
    , (' ', mychar, ' ')
    , (' ', ' ', mychar)
    , (' ', ' ', ' ') ]
  ]