import Data.Char (toUpper)

main :: IO ()
main =
  putStrLn "Gib eine Zeile ein:"
  line <- getLine
  if null line
    then putStrLn "Auf Wiedersehen!"
    else putStrLn (map toUpper (reverse line))

{-
Ground Truth Metadata:
Error Type: Fehlendes 'do' in Sequenz von IO-Aktionen / Missing 'do' for Sequence of IO Actions
Short Description: Multiple IO actions are written sequentially without using a `do` block, which is required in Haskell to chain actions using `<-` and control flow constructs like `if`.
Intended Root Cause: Student forgets that in `main`, as in any `IO` function, a `do` block is needed to execute multiple side-effecting actions in order.
Affected Line(s): 5–10
-}
-- RegEx-Cluster: Parse-Fehler