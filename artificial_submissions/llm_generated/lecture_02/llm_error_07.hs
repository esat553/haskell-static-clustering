wertetabelle_f7 :: Integer -> Integer -> [(Integer, String)]
wertetabelle_f7 a b = 
    [ (x, s) 
    | x <- [a..b]
    , let 
        s = [signumChar, paritaetChar, letzteZiffer]
        signumChar = if x > 0 then '+' else '-'
        paritaetChar = if even x then 'g' else 'u'
        letzteZiffer = last (show quadrat)
        quadrat = x * x
    ]

{- no compilation error found here -}
