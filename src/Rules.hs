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
    54  -> applyRule54 a b c
    60  -> applyRule60 a b c
    62  -> applyRule62 a b c
    90  -> applyRule90 a b c
    94  -> applyRule94 a b c
    102 -> applyRule102 a b c
    110 -> applyRule110 a b c
    122 -> applyRule122 a b c
    126 -> applyRule126 a b c
    150 -> applyRule150 a b c
    158 -> applyRule158 a b c
    182 -> applyRule182 a b c
    188 -> applyRule188 a b c
    190 -> applyRule190 a b c
    220 -> applyRule220 a b c
    222 -> applyRule122 a b c
    250 -> applyRule122 a b c

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

applyRule54 :: Char -> Char -> Char -> Char
applyRule54 a b c = case (a, b, c) of
    ('*', '*', '*') -> ' '
    ('*', '*', ' ') -> ' '
    ('*', ' ', '*') -> '*'
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> ' '
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule60 :: Char -> Char -> Char -> Char
applyRule60 a b c = case (a, b, c) of
    ('*', '*', '*') -> ' '
    ('*', '*', ' ') -> ' '
    ('*', ' ', '*') -> '*'
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> ' '
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule62 :: Char -> Char -> Char -> Char
applyRule62 a b c = case (a, b, c) of
    ('*', '*', '*') -> ' '
    ('*', '*', ' ') -> ' '
    ('*', ' ', '*') -> '*'
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule94 :: Char -> Char -> Char -> Char
applyRule94 a b c = case (a, b, c) of
    ('*', '*', '*') -> ' '
    ('*', '*', ' ') -> '*'
    ('*', ' ', '*') -> ' '
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule102 :: Char -> Char -> Char -> Char
applyRule102 a b c = case (a, b, c) of
    ('*', '*', '*') -> ' '
    ('*', '*', ' ') -> '*'
    ('*', ' ', '*') -> '*'
    ('*', ' ', ' ') -> ' '
    (' ', '*', '*') -> ' '
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule122 :: Char -> Char -> Char -> Char
applyRule122 a b c = case (a, b, c) of
    ('*', '*', '*') -> ' '
    ('*', '*', ' ') -> '*'
    ('*', ' ', '*') -> '*'
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> ' '
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule126 :: Char -> Char -> Char -> Char
applyRule126 a b c = case (a, b, c) of
    ('*', '*', '*') -> ' '
    ('*', '*', ' ') -> '*'
    ('*', ' ', '*') -> '*'
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule150 :: Char -> Char -> Char -> Char
applyRule150 a b c = case (a, b, c) of
    ('*', '*', '*') -> '*'
    ('*', '*', ' ') -> ' '
    ('*', ' ', '*') -> ' '
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> ' '
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule158 :: Char -> Char -> Char -> Char
applyRule158 a b c = case (a, b, c) of
    ('*', '*', '*') -> '*'
    ('*', '*', ' ') -> ' '
    ('*', ' ', '*') -> ' '
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule182 :: Char -> Char -> Char -> Char
applyRule182 a b c = case (a, b, c) of
    ('*', '*', '*') -> '*'
    ('*', '*', ' ') -> ' '
    ('*', ' ', '*') -> '*'
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> ' '
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule188 :: Char -> Char -> Char -> Char
applyRule188 a b c = case (a, b, c) of
    ('*', '*', '*') -> '*'
    ('*', '*', ' ') -> ' '
    ('*', ' ', '*') -> '*'
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> ' '
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule190 :: Char -> Char -> Char -> Char
applyRule190 a b c = case (a, b, c) of
    ('*', '*', '*') -> '*'
    ('*', '*', ' ') -> ' '
    ('*', ' ', '*') -> '*'
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule220 :: Char -> Char -> Char -> Char
applyRule220 a b c = case (a, b, c) of
    ('*', '*', '*') -> '*'
    ('*', '*', ' ') -> '*'
    ('*', ' ', '*') -> ' '
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> ' '
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule222 :: Char -> Char -> Char -> Char
applyRule222 a b c = case (a, b, c) of
    ('*', '*', '*') -> '*'
    ('*', '*', ' ') -> '*'
    ('*', ' ', '*') -> ' '
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> '*'
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '

applyRule250 :: Char -> Char -> Char -> Char
applyRule250 a b c = case (a, b, c) of
    ('*', '*', '*') -> '*'
    ('*', '*', ' ') -> '*'
    ('*', ' ', '*') -> '*'
    ('*', ' ', ' ') -> '*'
    (' ', '*', '*') -> '*'
    (' ', '*', ' ') -> ' '
    (' ', ' ', '*') -> '*'
    (' ', ' ', ' ') -> ' '
    _               -> ' '

