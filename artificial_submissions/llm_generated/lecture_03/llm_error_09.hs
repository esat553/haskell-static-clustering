describeTripleList :: [(Int, Int, Int)] -> [String]
describeTripleList = map describeTriple

describeTriple :: (Int, Int, Int) -> String
describeTriple t =
  let
    (a,b,c) = t
    isNull = a == 0 && b == 0 && c == 0
  if isNull then "Nulltupel" else "Gemischt"

{-
Ground Truth Metadata:
Error Type: Fehlender Ausdrucksbestandteil / Missing 'in' in let-expression
Short Description: A `let` binding is used without a corresponding `in` clause, which is required to introduce the expression that uses the bound variables.
Intended Root Cause: Student forgets that in Haskell, `let` must be followed by `in` to form a complete expression and incorrectly assumes that `let` behaves like a block scope.
Affected Line(s): 6
-}
-- RegEx-Cluster: Parse-Fehler