exampleVariable :: Int
exampleVariable :: Double
exampleVariable = 5

{-
error:
    Duplicate type signatures for ‘exampleVariable’
    at 1:1-15
       2:1-15
  |
2 | exampleVariable :: Double
  | ^^^^^^^^^^^^^^^
-}