wertetabelle_f9 :: Integer -> Integer -> [(Integer, String)]
wertetabelle_f9 a b = [ result | x <- [a..b], do
    let sign = if x > 0 then '+' else '0'
    let parity = if even x then 'g' else 'u'
    let s = [sign, parity, '9']
    let result = (x, s)
]

{-
Ground Truth Metadata:
Error Type: `do`-Notation ohne monadischen Kontext / `do` Notation Without Monadic Context
Short Description: The `do` keyword is used within a list comprehension, which is not a monadic context and therefore syntactically invalid. `let` bindings in list comprehensions should be listed with commas, not sequenced via `do`.
Intended Root Cause: Student mistakenly assumes that `do` can be used to sequence `let` bindings in any context, not realizing that `do` is specific to monadic expressions.
Affected Line(s): 3
-}
-- RegEx-Cluster: Parse-Fehler