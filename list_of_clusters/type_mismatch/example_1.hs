exampleFunction :: Int -> Bool
exampleFunction x = x

{-
error:
    • Couldn't match expected type ‘Bool’ with actual type ‘Int’
    • In the expression: x
      In an equation for ‘exampleFunction’: exampleFunction x = x
  |
2 | exampleFunction x = x
  |                     ^
-}