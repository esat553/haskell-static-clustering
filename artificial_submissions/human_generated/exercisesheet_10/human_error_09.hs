import System.IO

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    hSetBuffering handle LineBuffering
    contents <- hGetContents handle
    let linesList = lines contents
        numberedReversedLines = zipWith formatLine [1..] linesList
    map putStrLn numberedReversedLines

formatLine :: Int -> String -> String
formatLine n line = show n ++ ": " ++ reverse line

{-
Ground Truth Metadata:
Error Type: Falsche Funktionsverwendung / Incorrect Function Usage
Short Description: The function 'map' is used instead of 'mapM_' to print each line, resulting in a list of actions rather than executing the output actions sequentially.
Error Location: In the 'main' function, line with 'map putStrLn numberedReversedLines' (line 10).
Intended Root Cause: Student confuses pure mapping functions with monadic actions in Haskell and does not recognize that printing requires sequencing of IO actions using 'mapM_'.
Affected Line(s): 10
-}
-- RegEx-Cluster: Typenkonflikt