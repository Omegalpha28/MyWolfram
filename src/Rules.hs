module Rules where

applyRule :: Int -> String -> String
applyRule rule line = applyRuleGeneric rule (" " ++ line ++ " ") ""

applyRuleGeneric :: Int -> String -> String -> String
applyRuleGeneric rule [_, _] acc = acc
applyRuleGeneric rule (x:y:z:rest) acc =
    applyRuleGeneric rule (y:z:rest) (acc ++ [applyRuleToNeighbors rule x y z])
applyRuleGeneric _ _ acc = acc

applyRuleToNeighbors :: Int -> Char -> Char -> Char -> Char
applyRuleToNeighbors rule a b c = case rule of
    30  -> applyRule30 a b c
    90  -> applyRule90 a b c
    110 -> applyRule110 a b c
    _   -> ' '

applyRule30 :: Char -> Char -> Char -> Char
applyRule30 a b c = case (a, b, c) of
    ('*', '*', '*') -> ' '
    ('*', '*', ' ') -> ' '
    ('*', ' ', '*') -> ' '
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '


applyRule90 :: Char -> Char -> Char -> Char
applyRule90 a b c = case (a, b, c) of
    ('*', '*', '*') -> ' '
    ('*', '*', ' ') -> '*'
    ('*', ' ', '*') -> ' '
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> ' '
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule110 :: Char -> Char -> Char -> Char
applyRule110 a b c = case (a, b, c) of
    ('*', '*', '*') -> ' '
    ('*', '*', ' ') -> '*'
    ('*', ' ', '*') -> '*'
    ('*', ' ', ' ') -> ' '
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '
