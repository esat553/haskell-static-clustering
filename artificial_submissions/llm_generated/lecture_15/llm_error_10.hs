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
Error Type: Fehlender Import / Missing Import
Short Description: The required import statement for `System.IO` is missing, making functions like `withFile`, `hSetBuffering`, and `hGetLine` unavailable.
Intended Root Cause: Student forgets to include necessary module imports for used IO functions.
Affected Line(s): 1
-}
-- RegEx-Cluster: Typenkonstruktor oder Klasse nicht definiert