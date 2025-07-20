import System.IO

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    hSetBuffering handle LineBuffering
    contents <- hGetContents handle
    linesList = lines contents
    numberedReversedLines = zipWith formatLine [1..] linesList
    mapM_ putStrLn numberedReversedLines

formatLine :: Int -> String -> String
formatLine n line = show n ++ ": " ++ reverse line

{-
Ground Truth Metadata:
Error Type: Fehlendes Schlüsselwort / Missing Keyword ('let')
Short Description: The 'let' keyword is missing before variable assignments in the 'do' block, causing a syntax error.
Error Location: Inside the 'main' function, in the 'do' block (lines 7–10).
Intended Root Cause: Student misunderstands how variable bindings work in Haskell's 'do' notation and omits the required 'let'.
Affected Line(s): 7–10
-}
-- RegEx-Cluster: Parse-Fehler
