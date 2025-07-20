import System.IO

main :: IO ()
main = withFile "input.txt" ReadMode $ \handle ->
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
Error Type: Fehlendes 'do' / Missing 'do' Keyword
Short Description: The `do` keyword is missing before a sequence of actions inside the lambda expression passed to `withFile`, causing syntactic confusion.
Intended Root Cause: Student is unaware that multiple IO actions in a lambda must be grouped using `do` notation and forgets to insert it.
Affected Line(s): after 4
-}
-- RegEx-Cluster: Typenkonflikt