import Data.Char (toUpper)

main :: IO ()
main = loop

loop :: IO ()
loop = do
  putStrLn "Gib eine Zeile ein:"
  line <- getLine
  if null line
  then putStrLn "Auf Wiedersehen!"
  else do
  let reversedLine = reverse line
      putStrLn (map toUpper reversedLine)
      loop

{-
Ground Truth Metadata:
Error Type: Falsche Einrückung in do-Notation / Incorrect Indentation in do-Notation
Short Description: The `then` and `else` branches of the `if` expression are incorrectly indented, and the `let` and subsequent expressions in the `else` block are not properly aligned, violating Haskell’s layout rules.
Intended Root Cause: Student misunderstands the significance of indentation in `do`-notation and places the `let` block at the wrong indentation level, causing a syntax or parse error.
Affected Line(s): 8–11
-}
-- RegEx-Cluster: Parse-Fehler