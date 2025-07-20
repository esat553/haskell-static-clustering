dropLastN :: Int -> [a] -> [a]
dropLastN n xs =
    case xs of
        [] = []
        (y:ys) -> if n > 0 then dropLastN (n-1) ys else y:ys#
{-
Ground Truth Metadata:
Error Type: Falsche 'case'-Syntax / Incorrect 'case' Syntax
Short Description: In the `case` expression, the pattern `[]` is followed by `=`, but Haskell requires `->` to separate patterns from their corresponding expressions.
Intended Root Cause: Student confuses pattern matching syntax in `case` expressions with top-level function definitions and uses `=` instead of `->`.
Affected Line(s): 4
-}
-- RegEx-Cluster: Parse-Fehler