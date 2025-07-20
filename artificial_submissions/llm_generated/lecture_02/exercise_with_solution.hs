{-
Schreiben Sie eine Funktion, die für alle Zahlen zwischen zwei gegebenen Grenzen a und b eine Liste von Tupeln erzeugt. Jedes Tupel soll aus der Zahl selbst und einer dreistelligen Zeichenkette bestehen. Die Zeichenkette setzt sich wie folgt zusammen: Das erste Zeichen gibt das Vorzeichen der Zahl an ('+' für positive Zahlen, '-' für negative und '0' für null), das zweite Zeichen zeigt an, ob die Zahl gerade ('g') oder ungerade ('u') ist, und das dritte Zeichen entspricht der letzten Ziffer des Quadrats der Zahl. Die Funktion soll möglichst funktional und mit Haskell-typischen Mitteln umgesetzt werden. 
-}
{-
english translation:
Write a function that generates a list of tuples for all numbers between two given bounds a and b. Each tuple should consist of the number itself and a three-character string. The string is composed as follows: the first character indicates the sign of the number ('+' for positive numbers, '-' for negative, and '0' for zero), the second character shows whether the number is even ('g') or odd ('u'), and the third character corresponds to the last digit of the square of the number. The function should be implemented as functionally as possible and using typical Haskell constructs.
-}

wertetabelle :: Integer -> Integer -> [(Integer, String)]
wertetabelle a b = 
  [ (x, vorzeichen x : paritaet x : [letzteZiffer (x*x)])
  | x <- [a..b]
  ]
  where
    vorzeichen n
      | n > 0     = '+'
      | n < 0     = '-'
      | otherwise = '0'
    paritaet n
      | even n    = 'g'
      | otherwise = 'u'
    letzteZiffer n = last (show n)
