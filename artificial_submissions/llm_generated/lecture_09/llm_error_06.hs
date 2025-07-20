cleanVotes :: [String] -> [String]
cleanVotes = filter ( (length s >= 4) && (s /= "None") )

{-
Ground Truth Metadata:
Error Type: Ungültiger Variablenbezug / Invalid Variable Reference
Short Description: The expression refers to a variable `s` that is not bound in the scope of the predicate function, making it syntactically and semantisch ungültig.
Intended Root Cause: Student writes the predicate as if `s` were already defined, forgetting that `filter` expects a function that takes one argument.
Affected Line(s): 2
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich