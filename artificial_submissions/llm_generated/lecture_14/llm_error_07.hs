import Data.Char (toUpper)

main :: IO ()
main = do
  let loop = do
    line <- getLine
    if not (null line)
      then do
        putStrLn (map toUpper (reverse line))
        loop
      else putStrLn "Auf Wiedersehen!"

{-
Ground Truth Metadata:
Error Type: Lokale rekursive Definition ohne Fixpunkt / Local Recursive Definition Without Fixpoint
Short Description: The recursive function `loop` is defined using `let`, but in Haskell, `let` bindings are not recursive by default in a `do` block unless defined at the top level or using a fixpoint (e.g. `let loop = ... in loop`).
Intended Root Cause: Student defines a recursive function locally within a `do` block using `let`, but forgets that such definitions are non-recursive unless invoked appropriately or lifted to top level.
Affected Line(s): 4–11
-}
-- RegEx-Cluster: Parse-Fehler