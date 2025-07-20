import System.IO

main :: IO ()
main = withFile "input.txt" ReadMode $ \handle -> do
    hSetBuffering handle LineBuffering
processLines handle 1

processLines :: Handle -> Int -> IO ()
processLines handle n = do
    isEOF <- hIsEOF handle
    if isEOF
        then return ()
        else do
            line <- hGetLine handle
            putStrLn $ show n ++ ": " ++ reverse line
            processLines handle (n + 1)

{-
Ground Truth Metadata:
Error Type: Falsche Einrückung / Incorrect Indentation
Short Description: The line `processLines handle 1` is indented at the top level and no longer part of the `do` block inside the lambda expression passed to `withFile`.
Intended Root Cause: Student underestimates the importance of indentation in `do` blocks and places statements at an incorrect hierarchy level.
Affected Line(s): 6
-}
-- RegEx-Cluster: Ungültige Top-Level-Deklaration