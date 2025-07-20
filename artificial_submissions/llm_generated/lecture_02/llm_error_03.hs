wertetabelle_f3 :: Integer -> Integer -> [(Integer, String)]
wertetabelle_f3 a b = [(x, buildString x) | x <- [a..b]]

buildString :: Integer -> String
buildString x
    = x > 0 = ['+', 'g', '1']
    = x < 0 = ['-', 'u', '4']
    else      ['0', 'g', '0']

{-
Ground Truth Metadata:
Error Type: Falsche Syntax für Guards / Incorrect Guard Syntax
Short Description: The function `buildString` uses invalid guard syntax by writing conditions with `=` instead of `|`, and incorrectly uses `else` instead of `otherwise`.
Intended Root Cause: Student confuses guard syntax with `if-else` constructs or assignment-like expressions and does not follow Haskell’s guard conventions.
Affected Line(s): 7–9
-}
-- RegEx-Cluster: Parse-Fehler