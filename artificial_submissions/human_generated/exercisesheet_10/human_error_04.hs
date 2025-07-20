import System.IO

main :: IO 
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    hSetBuffering handle LineBuffering
    contents <- hGetContents handle
    let linesList = lines contents
        numberedReversedLines = zipWith formatLine [1..] linesList
    mapM_ putStrLn numberedReversedLines

formatLine :: Int -> String -> String
formatLine n line = show n ++ ": " ++ reverse line

{-
Ground Truth Metadata:
Error Type: Falsche Typannotation / Incorrect Type Annotation
Short Description: The type signature of 'main' is declared as 'IO' instead of 'IO ()', which is required in Haskell for the main entry point.
Error Location: Type signature of the 'main' function (line 4).
Intended Root Cause: Student does not understand that 'IO' is a type constructor and 'IO ()' specifies a concrete type; confusion between 'IO' as a monad and a concrete action type.
Affected Line(s): 4
-}
-- RegEx-Cluster: Falsche Anzahl von Typ-Argumenten
