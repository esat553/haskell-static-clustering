import System.IO

main :: IO ()
main = withFile ReadMode "input.txt" $ \handle -> do
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
Error Type: Falsche Argumentreihenfolge / Incorrect Argument Order
Short Description: The arguments `ReadMode` and `"input.txt"` are passed in the wrong order to `withFile`, which expects the file path first and then the mode.
Intended Root Cause: Student misremembers the order of parameters for the `withFile` function and reverses them.
Affected Line(s): 4
-}
-- RegEx-Cluster: Typenkonflikt