wertetabelle_f10 :: Integer -> Integer -> [(Integer, String)]
wertetabelle_f10 a b =
    [ (x, s) 
    | x <- [a..b]
    , let 
        sign = if x > 0 then '+'
               else if x < 0 then '-'
        par  = 'g'
        last = '1'
        s    = [sign, par, last]
    ]

{-
Ground Truth Metadata:
Error Type: Fehlender else-Zweig in if-then-else / Missing else Branch in if-then-else
Short Description: The `if` expression for determining `sign` lacks a final `else` branch to handle the case when `x == 0`, making the expression incomplete and ill-typed.
Intended Root Cause: Student forgets that Haskell's `if-then-else` is an expression that must cover all branches explicitly, unlike in some imperative languages where falling through is allowed.
Affected Line(s): after 7
-}
-- RegEx-Cluster: Parse-Fehler