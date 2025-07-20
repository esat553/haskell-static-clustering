{-
Definiere eine Funktion describeTripleList :: [(Int, Int, Int)] -> [String], die eine Liste von Zahlentupeln analysiert.
Gib zu jedem Tupel einen String zurück, der je nach Inhalt des Tupels eine der folgenden Aussagen trifft:
"Nulltupel", wenn alle drei Zahlen 0 sind
"Einheitsvektor", wenn genau eine der Zahlen 1 ist und die anderen 0
"Gleich", wenn alle drei Zahlen gleich sind (aber nicht 0)
"Gemischt", in allen anderen Fällen
Nutze Pattern Matching, Guards, und lokale Definitionen, wo es sinnvoll ist.
-}
{-
english translation:
Define a function describeTripleList :: [(Int, Int, Int)] -> [String] that analyzes a list of tuples of numbers.
For each tuple, return a string that states one of the following messages depending on the contents of the tuple:
"Zero tuple" if all three numbers are 0
"Unit vector" if exactly one of the numbers is 1 and the others are 0
"Equal" if all three numbers are equal (but not 0)
"Mixed" in all other cases
Use pattern matching, guards, and local definitions where appropriate.
-}
describeTripleList :: [(Int, Int, Int)] -> [String]
describeTripleList tuples = map describeTriple tuples

describeTriple :: (Int, Int, Int) -> String
describeTriple (0, 0, 0) = "Nulltupel"
describeTriple (a, b, c)
  | isUnitVector = "Einheitsvektor"
  | allEqual     = "Gleich"
  | otherwise    = "Gemischt"
  where
    isUnitVector = (a+b+c == 1) && (a*a + b*b + c*c == 1)
    allEqual = a == b && b == c