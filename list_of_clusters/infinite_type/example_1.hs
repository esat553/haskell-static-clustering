gerade :: Integral a => a -> Bool
gerade x = x div 2 * 2 == x
{-
error:
    • Occurs check: cannot construct the infinite type:
        a ~ (Integer -> Integer -> Integer) -> Integer -> a
    • The function ‘x’ is applied to two arguments,
      but its type ‘a’ has none
      In the first argument of ‘(*)’, namely ‘x div 2’
      In the first argument of ‘(==)’, namely ‘x div 2 * 2’
    • Relevant bindings include
        x :: a (bound at 2:8)
        gerade :: a -> Bool (bound at 2:1)
  |
2 | gerade x = x div 2 * 2 == x
  |            ^^^^^^^
-}