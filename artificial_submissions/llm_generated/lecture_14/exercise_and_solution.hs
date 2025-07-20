{-
Schreibe ein interaktives Programm, das Zeilen vom Benutzer einliest, sie jeweils umdreht und in Großbuchstaben ausgibt. Wenn der Benutzer eine leere Zeile eingibt, soll das Programm mit einer Abschiedszeile enden. Verwende ausschließlich die do-Notation.
-}
{-
english translation:
Write an interactive program that reads lines from the user, reverses each line, and outputs it in uppercase letters. When the user enters an empty line, the program should end with a farewell message. Use only the do-notation.
-}
import Data.Char (toUpper)

main :: IO ()
main = loop

loop :: IO ()
loop = do
  putStrLn "Gib eine Zeile ein (leer zum Beenden):"
  line <- getLine
  if null line
    then putStrLn "Auf Wiedersehen!"
    else do
      let reversedLine = reverse line
      let upperLine = map toUpper reversedLine
      putStrLn upperLine
      loop