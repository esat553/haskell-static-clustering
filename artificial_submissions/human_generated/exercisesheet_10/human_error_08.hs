import System.IO

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    hSetBuffering handle LineBuffering
    contents <- hGetContents handle
    let linesList = lines contents
        numberedReversedLines = zipWith formatLine [1..] linesList

formatLine :: Int -> String -> String
formatLine n line = show n ++ ": " ++ reverse line

{-
Ground Truth Metadata:
Error Type: Fehlende abschließende Anweisung im do-Block / Missing Final Statement in Do Block
Short Description: The final statement in the 'do' block is missing, so the processed output is not printed or returned.
Error Location: In the 'main' function, at the end of the 'do' block (after line 9).
Intended Root Cause: Student does not realize that every 'do' block in Haskell must end with an action (such as printing or returning a value), and omits the output statement.
Affected Line(s): End of 'main' do-block (after line 9)
-}
-- RegEx-Cluster: Letzte Anweisung im 'do'-Block
