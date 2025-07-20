import Data.Char (toUpper)

main :: IO ()
main = loop

loop :: IO ()
loop = do
  line <- getLine
  if null line
    then putStrLn "Auf Wiedersehen!"
    else
      let reversedLine = reverse line
      let upperLine = map toUpper reversedLine
      putStrLn upperLine
      loop

{-
Ground Truth Metadata:
Error Type: Fehlendes 'do' im else-Zweig / Missing 'do' Block in Else Branch
Short Description: Multiple actions are listed in the `else` branch of an `if` expression, but only a single expression is allowed unless explicitly grouped using `do`.
Intended Root Cause: Student assumes that indentation alone is sufficient to group multiple actions in an `if` branch, overlooking the requirement for an explicit `do` block.
Affected Line(s): 9–12
-}
-- RegEx-Cluster: Parse-Fehler