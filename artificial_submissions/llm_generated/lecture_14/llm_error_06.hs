import Data.Char (toUpper)

main :: IO ()
main = loop

loop :: IO ()
loop = do
  line <- getLine
  if null line
    then return "Auf Wiedersehen!"
    else do
      putStrLn (map toUpper (reverse line))
      loop

{-
Ground Truth Metadata:
Error Type: Rückgabetyp stimmt nicht mit Signatur überein / Return Type Does Not Match Signature
Short Description: The `then` branch returns a string wrapped in `IO` using `return`, resulting in `IO String`, while the expected type is `IO ()` (i.e., an action with no result value).
Intended Root Cause: Student confuses `return` in Haskell with printing and uses it to return a string, not realizing that `return` does not perform any visible output.
Affected Line(s): 8
-}
-- RegEx-Cluster: Typenkonflikt