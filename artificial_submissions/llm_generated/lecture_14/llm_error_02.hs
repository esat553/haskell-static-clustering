import Data.Char (toUpper)

main :: IO ()
main = do
  putStrLn "Gib eine Zeile ein:"
  let processedLine = map toUpper (reverse getLine)
  putStrLn processedLine

{-
Ground Truth Metadata:
Error Type: Falsche Anwendung reiner Funktionen auf IO-Werte / Invalid Application of Pure Functions to IO Values
Short Description: The expression `reverse getLine` attempts to apply a pure function to an IO action, which is not valid because `getLine` must first be executed to extract the `String` value.
Intended Root Cause: Student does not distinguish between pure values and IO actions and mistakenly uses `getLine` as if it directly returns a `String`.
Affected Line(s): 6
-}
-- RegEx-Cluster: Typenkonflikt