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
            let line = hGetLine handle
            putStrLn $ show n ++ ": " ++ reverse line
            processLines handle (n + 1)

{-
Ground Truth Metadata:
Error Type: Falscher Zuweisungsoperator / Incorrect Assignment Operator
Short Description: The line `let line = hGetLine handle` incorrectly binds an `IO String` using `let`, which does not perform IO actions. This results in a type mismatch when using `reverse line`.
Intended Root Cause: Student misunderstands the difference between binding a pure value with `let` and performing an IO action with `<-`, leading to improper handling of monadic values.
Affected Line(s): 14-15
-}
-- RegEx-Cluster: Typenkonflikt