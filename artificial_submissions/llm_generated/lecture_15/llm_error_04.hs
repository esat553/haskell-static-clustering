import System.IO

main :: IO ()
main = do
    withFile "input.txt" ReadMode $ \handle -> do
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
Error Type: Verwendung eines Bezeichners außerhalb seines Gültigkeitsbereichs / Use of Out-of-Scope Identifier
Short Description: The value `handle` is used outside the scope in which it was defined via `withFile`. Once the `withFile` block ends, `handle` is no longer accessible.
Intended Root Cause: Student assumes that a variable bound inside a lambda (in this case, `handle`) remains available after the block ends, not realizing the scoping rules of Haskell's `do` notation and resource management.
Affected Line(s): 7
-}
-- RegEx-Cluster: Variable nicht im Gültigkeitsbereich