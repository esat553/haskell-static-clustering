import Data.Char (toUpper)

main :: IO ()
main = do
  if not (null getLine)
    then do
      line <- getLine
      putStrLn (map toUpper (reverse line))
      main
    else
      putStrLn "Auf Wiedersehen!"

{-
Ground Truth Metadata:
Error Type: Bedingung auf IO-Aktion statt auf deren Ergebnis / Condition Applied to IO Action Instead of Its Result
Short Description: The condition `null getLine` applies `null` directly to the IO action `getLine`, instead of first executing `getLine` to obtain a `String`.
Intended Root Cause: Student confuses the IO action `getLine` with its result and applies pure functions to the action itself, leading to a type mismatch and redundant evaluation.
Affected Line(s): 5
-}
-- RegEx-Cluster: Fehlende Instanz