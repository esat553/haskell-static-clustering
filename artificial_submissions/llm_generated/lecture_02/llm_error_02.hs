wertetabelle_f2 :: Integer -> Integer -> [(Integer, String)]
wertetabelle_f2 a b = 
    [ (x, s) 
    | x <- [a..b],
      signum = if x > 0 then '+' else '-',
      s = [signum, 'g', '0']
    ]

{-
Ground Truth Metadata:
Error Type: Fehlendes `let` in der List-Comprehension / Missing `let` in List Comprehension
Short Description: The list comprehension includes variable bindings (`signum`, `s`) without the required `let` keyword, which is syntactically invalid in Haskell.
Intended Root Cause: Student applies an imperative or Python-like style and is unaware that Haskell requires `let` for local bindings within list comprehensions.
Affected Line(s): 3–7
-}
-- RegEx-Cluster: Parse-Fehler