wertetabelle_f5 :: Integer -> Integer -> [(Integer, String)]
wertetabelle_f5 a b =
    [ (x, [signumChar x, 'g', '1']) | x <- [a..b] ]

signumChar :: Integer -> Char
signumChar x =
    case x of
        > 0 -> '+'
        < 0 -> '-'
        0   -> '0'

{-
Ground Truth Metadata:
Error Type: Falsche Anwendung einer case-Anweisung / Incorrect Use of Case Expression
Short Description: The `case` expression in `signumChar` uses comparison operators (`>`, `<`) directly in patterns, which is not allowed. Patterns must be constructors or literal values.
Intended Root Cause: Student misinterprets how `case` works in Haskell and mistakenly tries to use boolean conditions instead of matching on specific values and delegating logic to guards or `if` expressions.
Affected Line(s): 8–10
-}
-- RegEx-Cluster: Parse-Fehler