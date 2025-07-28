{-
Schreibe ein Programm, das eine Datei zeilenweise einliest, jede Zeile rückwärts dreht und nummeriert ausgibt. Verwende withFile mit explizitem Handle und setze LineBuffering.
-}
{-
english translation:
Write a program that reads a file line by line, reverses each line, and outputs it with line numbers. Use withFile with an explicit handle and set LineBuffering.
-}
import System.IO

main :: IO ()
main = do
  withFile "input.txt" ReadMode $ \handle -> do
    hSetBuffering handle LineBuffering
    contents <- hGetContents handle
    let linesList = lines contents
        numberedReversedLines = zipWith formatLine [1..] linesList
    mapM_ putStrLn numberedReversedLines

formatLine :: Int -> String -> String
formatLine n line = show n ++ ": " ++ reverse line
