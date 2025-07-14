exampleCode :: Int -> Int
exampleCode :: Int -> Int
exampleCode x = 4
{-
error:
    Duplicate type signatures for ‘exampleCode’
    at 1:1-11
       2:1-11
  |
2 | exampleCode :: Int -> Int
  | ^^^^^^^^^^^
-}