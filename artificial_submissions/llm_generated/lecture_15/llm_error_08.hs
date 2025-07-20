import System.IO

main :: IO ()
main = withFile "input.txt" ReadMode $ \handle -> do
    hSetBuffering handle LineBuffering
    processLines handle 1

processLines :: Handle -> Int -> IO ()
processLines handle n = do
    isEOF <- hIsEOF handle
    if isEOF
        then
        else do
            line <- hGetLine handle
            putStrLn $ show n ++ ": " ++ reverse line
            processLines handle (n + 1)

{-
Ground Truth Metadata:
Error Type: Inkompatible Typen im if/then/else / Incompatible Types in if/then/else
Short Description: The `then` branch of the `if` expression is missing and would yield a pure value `()` instead of an `IO ()`, creating a type mismatch with the `else` branch.
Intended Root Cause: Student omits the `then` branch or writes an incomplete `if` expression, not realizing that all branches must return compatible types.
Affected Line(s): 12-13
-}
-- RegEx-Cluster: Parse-Fehler