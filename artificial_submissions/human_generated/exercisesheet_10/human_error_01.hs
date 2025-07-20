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
Error Type: Fehlender Import / Missing Import Statement
Short Description: The import statement for 'System.IO' is missing, causing functions like 'withFile', 'hSetBuffering', and 'hGetContents' to be undefined.
Error Location: Before the 'main' function (top of the file).
Intended Root Cause: Student forgets to import required modules.
Affected Line(s): 1
-}
-- RegEx-Cluster: Funktion nicht definiert
