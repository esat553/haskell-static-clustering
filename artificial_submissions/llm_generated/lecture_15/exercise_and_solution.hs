{-
Schreibe ein Programm, das eine Datei zeilenweise einliest, jede Zeile rückwärts dreht und nummeriert ausgibt. Verwende withFile mit explizitem Handle und setze LineBuffering.
-}
{-
english translation:
Write a program that reads a file line by line, reverses each line, and outputs it with line numbers. Use withFile with an explicit handle and set LineBuffering.
-}
import System.IO

main :: IO ()
main = withFile "input.txt" ReadMode $ \handle -> do
    hSetBuffering handle LineBuffering
    processLines handle 1

processLines :: Handle -> Int -> IO ()
processLines handle n = do
    isEOF <- hIsEOF handle
    if isEOF
        then return () -- Base case: end of file
        else do
            line <- hGetLine handle
            putStrLn $ show n ++ ": " ++ reverse line
            processLines handle (n + 1)