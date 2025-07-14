foo :: Int −> Int
foo x = x

{-
error:
    Not in scope: type constructor or class ‘−>’
  |
1 | foo :: Int −> Int
  |        ^^^^^^^^^^
error:
    Illegal operator ‘−>’ in type ‘Int −> Int’
      Use TypeOperators to allow operators in types
  |
1 | foo :: Int −> Int
  |        ^^^^^^^^^^
-}