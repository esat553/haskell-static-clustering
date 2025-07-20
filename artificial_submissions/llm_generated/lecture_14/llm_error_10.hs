import Data.Char (toUpper)

main :: IO ()
main = do
  line <- getLine
  if null line
    then putStrLn "Auf Wiedersehen!"
    else do
      let processed = map toUpper (reverse line)
      map putStrLn processed
      main

{-
Ground Truth Metadata:
Error Type: Falscher Gebrauch von map mit IO-Aktion / Incorrect Use of map with IO Action
Short Description: `map putStrLn processed` tries to apply `putStrLn` to each character of the reversed line, resulting in `[IO ()]`, which is not an `IO ()`. This is a misuse of `map` in a `do` block.
Error Location: Line with `map putStrLn processed`
Intended Root Cause: Student confuses `map` over `[Char]` with directly printing a `String`. The intention was likely to print the processed string, not its characters individually.
Fix: Replace `map putStrLn processed` with `putStrLn processed`
-}
-- RegEx-Cluster: Typenkonflikt