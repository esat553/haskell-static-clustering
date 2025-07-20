import Data.Char (toUpper)

main :: IO ()
main = do
  let line = getLine
  if null line
    then putStrLn "Auf Wiedersehen!"
    else do
      putStrLn (map toUpper (reverse line))
      main

{-
Ground Truth Metadata:
Error Type: Falscher Umgang mit IO-Werten / Incorrect Handling of IO Values
Short Description: The expression `let line = getLine` binds an IO action to a variable without extracting its result. As a result, `line` has type `IO String`, but `reverse` expect a `String`.
Intended Root Cause: Student forgets that IO actions must be unpacked using `<-` in do-blocks and incorrectly treats `getLine` as if it were a pure value.
Affected Line(s): 9
-}
-- RegEx-Cluster: Typenkonflikt