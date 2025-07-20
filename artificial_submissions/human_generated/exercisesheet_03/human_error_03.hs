take' z [] = []
take' z (x:xs)  | z <= 0 = []
                | otherwise = x : (take' (z - 1) xs)
-- tail-recursive
take'' z xs = tk z xs []
    where tk z [] acc = acc
          tk z (x:xs) acc   | z <= 0 = acc
                            | otherwise = tk (z - 1) xs (acc + [x])

{-
no compilation error
-}
