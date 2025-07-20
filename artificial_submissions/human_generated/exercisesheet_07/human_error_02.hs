foo = sqrt . (5 *) . isSubmapOfBy . flip take [1..50]

{-
Ground Truth Metadata:
Error Type: Verwendung undefinierter Bezeichner / Use of Undefined Identifier
Short Description: The function 'isSubmapOfBy' is used, but it is not defined or imported, leading to an unresolved identifier error.
Intended Root Cause: Student uses a function from a package or module without importing it, or mistakenly assumes it exists in the current scope.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 1
-}
-- RegEx-Cluster: Funktion nicht definiert