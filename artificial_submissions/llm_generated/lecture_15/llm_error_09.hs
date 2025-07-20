import System.IO

main :: IO ()
main = withFile "input.txt" ReadMode $ \handle ->
    hSetBuffering handle LineBuffering >> processLines handle 1

processLines :: Handle -> Int -> IO ()
processLines handle n = do
    isEOF <- hIsEOF handle
    if isEOF
        then return ()
        else do
            line <- hGetLine handle
            putStrLn $ show n ++ ": " ++ reverse line
            processLines handle (n + 1)

{- cant find a compilation problem here -}
