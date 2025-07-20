import System.IO

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    hSetBuffering handle LineBuffering
    content <- hGetContents handle
    let linesList = lines contents
        numberedReversedLines = zipWith formatLine [1..] linesList
    mapM_ putStrLn numberedReversedLines

formatLine :: Int -> String -> String
formatLine n line = show n ++ ": " ++ reverse line

{-
Ground Truth Metadata:
Error Type: Verwendung undefinierter Bezeichner / Use of Undefined Identifier
Short Description: The variable 'content' is used instead of 'contents', resulting in an undefined identifier when accessing 'lines contents' in the let binding.
Error Location: In the 'main' function, assignment and usage of 'contents' (lines 8).
Intended Root Cause: Student makes a typo in the variable name and does not notice the inconsistency between definition and usage.
Affected Line(s): 8
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich