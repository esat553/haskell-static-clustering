{-
Implementieren Sie das Sieb des Eratosthenes in Haskell. Nennen Sie die Funktion sieve', der einzige Parameter der Funktion soll eine Liste von Integer-Werten sein. Benutzen Sie dabei eine List Comprehension
-}
{-
english translation:
Implement the Sieve of Eratosthenes in Haskell. Name the function sieve'; the only parameter of the function should be a list of integer values. Use a list comprehension.
-}
sieve' :: [Integer] -> [Integer]
sieve' [] = []
sieve' (x:xs) = x : sieve' [y | y <- xs, y `mod` x /= 0]