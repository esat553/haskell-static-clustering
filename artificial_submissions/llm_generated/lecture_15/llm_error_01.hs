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
            putStrLn $ n ++ ": " ++ reverse line
            processLines handle (n + 1)

{-
Ground Truth Metadata:
Error Type: Typenkonflikt bei String-Verkettung / Type Mismatch in String Concatenation
Short Description: The expression `n ++ ": "` causes an error because `n` is of type `Int`, but `++` expects both operands to be of type `[Char]` (String).
Intended Root Cause: Student mistakenly assumes that an `Int` can be concatenated directly with a `String` using `++` without converting it to a `String` using `show`.
Affected Line(s): 11
-}
-- RegEx-Cluster: Typenkonflikt