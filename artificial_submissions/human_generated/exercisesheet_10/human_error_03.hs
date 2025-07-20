import System.IO

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    hSetBuffering handle LineBuffering
    contents <- hGetContents handle
    let linesList = lines contents
        numberedReversedLines = zipWith formatLine [1..] linesList
        mapM_ putStrLn numberedReversedLines

formatLine :: Int -> String -> String
formatLine n line = show n ++ ": " ++ reverse line

{-
Ground Truth Metadata:
Error Type: Einrückungsfehler / Indentation Error
Short Description: The 'mapM_ putStrLn numberedReversedLines' line is indented too far and is placed under the 'let' binding instead of being at the same level as 'let' within the 'do' block. This results in a parse error or incorrect scoping.
Error Location: Inside the 'main' function, in the 'do' block (line 10).
Intended Root Cause: Student forgot to format it right or misunderstood the Haskell layout and that the mapM_ line is on the same level as the let
Affected Line(s): 10
-}
-- RegEx-Cluster: Parse-Fehler in Funktionsdeklaration
