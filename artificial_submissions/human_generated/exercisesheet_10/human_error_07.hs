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
Short Description: The 'withFile' line is indented further to the right than necessary and does not align with the intended start of the 'do' block.
Error Location: In the 'main' function, first statement in the 'do' block (line 5).
Intended Root Cause: Student is not familiar with Haskell's indentation conventions and incorrectly indents the first statement in the 'do' block.
Affected Line(s): 5
-}
-- RegEx-Cluster: Leerer do-Block