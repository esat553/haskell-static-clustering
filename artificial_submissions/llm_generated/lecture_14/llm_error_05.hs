main :: IO ()
main = do
  line <- getLine
  if null line
    then putStrLn "Auf Wiedersehen!"
    else do
      putStrLn (map toUpper (reverse line))
      main

{-
Ground Truth Metadata:
Error Type: Fehlender Import für Standardfunktion / Missing Import for Standard Function
Short Description: The function `toUpper` is used without importing it from `Data.Char`, so it is not in scope and cannot be resolved during compilation.
Intended Root Cause: Student assumes `toUpper` is a built-in or always available function and forgets to explicitly import the module that defines it.
Affected Line(s): 7
-}
-- RegEx-Cluster: Funktion nicht definiert