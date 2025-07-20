wertetabelle_f1 :: Integer -> Integer -> [(Integer, String)]
wertetabelle_f1 a b = 
    [ (x, signumStr ++ paritaetStr ++ letzteZiffer)
    | x <- [a..b]
    , let 
        signumStr = if x > 0 then '+' else '-'
        paritaetStr = if even x then 'g' else 'u'
        letzteZiffer = (x*x) `mod` 10
    ]

{-
Ground Truth Metadata:
Error Type: Typkonflikt bei Zeichenverkettung / Type Mismatch in Character Concatenation
Short Description: The expression attempts to concatenate `Char` values and an `Int` using the `++` operator, which is only valid for `[Char]` (i.e., Strings). `signumStr` and `paritaetStr` are `Char`s, and `letzteZiffer` is of type `Int`, making the expression ill-typed.
Intended Root Cause: Student confuses `Char` with `String` and does not recognize that all elements in a string concatenation must be `[Char]`. Additionally, numeric values must be converted using `show` or `intToDigit`.
Affected Line(s): 3
-}
-- RegEx-Cluster: Typenkonflikt