import Control.Exception

readUserfile :: IO String
readUserfile = do
    print "Dateiname eingeben: " 
    name <- getLine
    inhalt <- try (readFile name) :: IO (Either SomeException String)
    case inhalt of
        Left ausnahme -> print (show ausnahme)
        Right wert -> return inhalt
{-
error:
    • Couldn't match type ‘()’ with ‘[Char]’
      Expected type: IO String
        Actual type: IO ()
    • In the expression: print (show ausnahme)
      In a case alternative: Left ausnahme -> print (show ausnahme)
      In a stmt of a 'do' block:
        case inhalt of
          Left ausnahme -> print (show ausnahme)
          Right wert -> return inhalt
  |
9 |         Left ausnahme -> print (show ausnahme)
  |                          ^^^^^^^^^^^^^^^^^^^^^

error:
    • Couldn't match type ‘Either SomeException String’ with ‘[Char]’
      Expected type: IO String
        Actual type: IO (Either SomeException String)
    • In the expression: return inhalt
      In a case alternative: Right wert -> return inhalt
      In a stmt of a 'do' block:
        case inhalt of
          Left ausnahme -> print (show ausnahme)
          Right wert -> return inhalt
   |
10 |         Right wert -> return inhalt
   |                       ^^^^^^^^^^^^^
-}