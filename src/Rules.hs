module Rules where

applyRule :: Int -> String -> Char -> String
applyRule rule line c = applyRuleGeneric rule (" " ++ line ++ " ") "" c

applyRuleGeneric :: Int -> String -> String -> Char -> String
applyRuleGeneric rule (x:y:z:rest) acc c =
    applyRuleGeneric rule (y:z:rest) (acc ++ [applyRuleToNeighbors rule x y z c]) c
applyRuleGeneric _ [] acc _ = acc
applyRuleGeneric _ _ acc _ = acc

applyRuleToNeighbors :: Int -> Char -> Char -> Char -> Char -> Char
applyRuleToNeighbors rule a b c d = case rule of
    30  -> applyRule30 a b c d
    54  -> applyRule54 a b c d
    60  -> applyRule60 a b c d
    62  -> applyRule62 a b c d
    90  -> applyRule90 a b c d
    94  -> applyRule94 a b c d
    102 -> applyRule102 a b c d
    110 -> applyRule110 a b c d
    122 -> applyRule122 a b c d
    126 -> applyRule126 a b c d
    150 -> applyRule150 a b c d
    158 -> applyRule158 a b c d
    182 -> applyRule182 a b c d
    188 -> applyRule188 a b c d
    190 -> applyRule190 a b c d
    220 -> applyRule220 a b c d
    222 -> applyRule222 a b c d
    250 -> applyRule250 a b c d
    _   -> ' '

applyRule30 :: Char -> Char -> Char -> Char -> Char
applyRule30 a b c character =
    if c == ' ' && a /= ' ' && b /= ' ' then ' '
    else if b == ' '  && a /= ' ' && c /= ' ' then ' '
    else if a == ' ' && c /= ' ' && b /= ' ' then character
    else if a == ' ' && b == ' ' && c /= ' ' then character
    else if b == ' ' && c == ' ' && a /= ' ' then character
    else if a == ' ' && c == ' ' && b /= ' ' then character
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '


applyRule90 :: Char -> Char -> Char -> Char -> Char
applyRule90 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then ' '
    else if a /= ' ' && b /= ' ' && c == ' ' then d
    else if a /= ' ' && b == ' ' && c /= ' ' then ' '
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then ' '
    else if a == ' ' && b == ' ' && c /= ' ' then d
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '


applyRule110 :: Char -> Char -> Char -> Char -> Char
applyRule110 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then ' '
    else if a /= ' ' && b /= ' ' && c == ' ' then d
    else if a /= ' ' && b == ' ' && c /= ' ' then d
    else if a /= ' ' && b == ' ' && c == ' ' then ' '
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then d
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '


applyRule54 :: Char -> Char -> Char -> Char -> Char
applyRule54 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then ' '
    else if a /= ' ' && b /= ' ' && c == ' ' then ' '
    else if a /= ' ' && b == ' ' && c /= ' ' then d
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then ' '
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then d
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '


applyRule60 :: Char -> Char -> Char -> Char -> Char
applyRule60 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then ' '
    else if a /= ' ' && b /= ' ' && c == ' ' then ' '
    else if a /= ' ' && b == ' ' && c /= ' ' then d
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then ' '
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '

applyRule62 :: Char -> Char -> Char -> Char -> Char
applyRule62 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then ' '
    else if a /= ' ' && b /= ' ' && c == ' ' then ' '
    else if a /= ' ' && b == ' ' && c /= ' ' then d
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then d
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '


applyRule94 :: Char -> Char -> Char -> Char -> Char
applyRule94 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then ' '
    else if a /= ' ' && b /= ' ' && c == ' ' then d
    else if a /= ' ' && b == ' ' && c /= ' ' then ' '
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then d
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '

applyRule102 :: Char -> Char -> Char -> Char -> Char
applyRule102 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then ' '
    else if a /= ' ' && b /= ' ' && c == ' ' then d
    else if a /= ' ' && b == ' ' && c /= ' ' then d
    else if a /= ' ' && b == ' ' && c == ' ' then ' '
    else if a == ' ' && b /= ' ' && c /= ' ' then ' '
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then d
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '

applyRule122 :: Char -> Char -> Char -> Char -> Char
applyRule122 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then ' '
    else if a /= ' ' && b /= ' ' && c == ' ' then d
    else if a /= ' ' && b == ' ' && c /= ' ' then d
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then ' '
    else if a == ' ' && b == ' ' && c /= ' ' then d
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '

applyRule126 :: Char -> Char -> Char -> Char -> Char
applyRule126 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then ' '
    else if a /= ' ' && b /= ' ' && c == ' ' then d
    else if a /= ' ' && b == ' ' && c /= ' ' then d
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then d
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '

applyRule150 :: Char -> Char -> Char -> Char -> Char
applyRule150 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then ' '
    else if a /= ' ' && b /= ' ' && c == ' ' then ' '
    else if a /= ' ' && b == ' ' && c /= ' ' then d
    else if a /= ' ' && b == ' ' && c == ' ' then ' '
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then ' '
    else if a == ' ' && b == ' ' && c == ' ' then d
    else ' '

applyRule158 :: Char -> Char -> Char -> Char -> Char
applyRule158 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then d
    else if a /= ' ' && b /= ' ' && c == ' ' then ' '
    else if a /= ' ' && b == ' ' && c /= ' ' then ' '
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then d
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '

applyRule182 :: Char -> Char -> Char -> Char -> Char
applyRule182 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then d
    else if a /= ' ' && b /= ' ' && c == ' ' then ' '
    else if a /= ' ' && b == ' ' && c /= ' ' then d
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then ' '
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then d
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '

applyRule188 :: Char -> Char -> Char -> Char -> Char
applyRule188 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then d
    else if a /= ' ' && b /= ' ' && c == ' ' then ' '
    else if a /= ' ' && b == ' ' && c /= ' ' then d
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then ' '
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '

applyRule190 :: Char -> Char -> Char -> Char -> Char
applyRule190 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then d
    else if a /= ' ' && b /= ' ' && c == ' ' then ' '
    else if a /= ' ' && b == ' ' && c /= ' ' then d
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then d
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '

applyRule220 :: Char -> Char -> Char -> Char -> Char
applyRule220 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then d
    else if a /= ' ' && b /= ' ' && c == ' ' then d
    else if a /= ' ' && b == ' ' && c /= ' ' then ' '
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then ' '
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '

applyRule222 :: Char -> Char -> Char -> Char -> Char
applyRule222 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then d
    else if a /= ' ' && b /= ' ' && c == ' ' then d
    else if a /= ' ' && b == ' ' && c /= ' ' then ' '
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then d
    else if a == ' ' && b == ' ' && c /= ' ' then d
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '

applyRule250 :: Char -> Char -> Char -> Char -> Char
applyRule250 a b c d =
    if a /= ' ' && b /= ' ' && c /= ' ' then d
    else if a /= ' ' && b /= ' ' && c == ' ' then d
    else if a /= ' ' && b == ' ' && c /= ' ' then d
    else if a /= ' ' && b == ' ' && c == ' ' then d
    else if a == ' ' && b /= ' ' && c /= ' ' then d
    else if a == ' ' && b /= ' ' && c == ' ' then ' '
    else if a == ' ' && b == ' ' && c /= ' ' then d
    else if a == ' ' && b == ' ' && c == ' ' then ' '
    else ' '
