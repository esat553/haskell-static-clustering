length' :: Num a => [a] -> a
length' x = sum (map (\x->1) x)

import Data.Char

someFunction :: String -> String
someFunction string = map (\x -> toUpper (x !! 0) ) (words string)

{-
error: parse error on input ‘import’
  |
4 | import Data.Char
  | ^^^^^^
  -}