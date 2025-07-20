import System.IO

main :: IO ()
main = withFile "input.txt" ReadMode $ \handle -> do
    hSetBuffering handle LineBuffering
    processLines handle 1

processLines :: Handle -> Int -> String
processLines handle n = do
    isEOF <- hIsEOF handle
    if isEOF
        then "Fertig"
        else do
            line <- hGetLine handle
            putStrLn $ show n ++ ": " ++ reverse line

{-
Ground Truth Metadata:
Error Type: Reine und unreine Pfade vermischt / Mixed Pure and Impure Code Paths
Short Description: The function `processLines` is declared to return a `String`, but its implementation includes `do` blocks with IO actions, resulting in an inconsistent return type.
Intended Root Cause: Student does not understand the distinction between pure and impure (IO) return types in Haskell and attempts to mix them within a single function body.
Affected Line(s): 6
-}
-- RegEx-Cluster: Typenkonflikt