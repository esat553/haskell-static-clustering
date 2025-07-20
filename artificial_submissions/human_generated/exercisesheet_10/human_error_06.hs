import System.IO

main :: IO ()
main = do
  withFile "input.txt" ReadMode $  -> do
    hSetBuffering handle LineBuffering
    contents <- hGetContents handle
    let linesList = lines contents
        numberedReversedLines = zipWith formatLine [1..] linesList
    mapM_ putStrLn numberedReversedLines

formatLine :: Int -> String -> String
formatLine n line = show n ++ ": " ++ reverse line

{-
Ground Truth Metadata:
Error Type: Fehlender Parameter in Lambda-Funktion / Missing Lambda Parameter
Short Description: The lambda function in the 'withFile' call is missing its required parameter ('handle'), leading to a syntax error and undefined variable.
Error Location: In the 'main' function, specifically the 'withFile' line (line 5).
Intended Root Cause: Student does not understand the syntax of lambda functions in Haskell and omits the parameter after '->', resulting in an incomplete function definition.
Affected Line(s): 5
-}
-- RegEx-Cluster: Parse-Fehler
