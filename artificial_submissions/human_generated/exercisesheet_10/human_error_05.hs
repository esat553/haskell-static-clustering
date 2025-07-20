import System.IO

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    hSetBuffering handle LineBuffering
    contents <- hGetContents handle
    let numberedReversedLines = zipWith formatLine [1..] contents
    mapM_ putStrLn numberedReversedLines

formatLine :: Int -> String -> String
formatLine n line = show n ++ ": " ++ reverse line

{-
Ground Truth Metadata:
Error Type: Typkonflikt / Type Mismatch
Short Description: The variable 'contents' is passed directly to 'zipWith formatLine', but 'contents' is a 'String' ([Char]) instead of a list of strings ([[Char]]). 'formatLine' expects a 'String' per line, but receives a 'Char'.
Error Location: In the 'main' function, assignment to 'numberedReversedLines' (line 8).
Intended Root Cause: Student does not recognize that 'hGetContents' returns the entire file as a single string and that 'lines contents' is needed to split it into individual lines for processing.
Affected Line(s): 8
-}
-- RegEx-Cluster: Typenkonflikt
