contains :: a -> [a] -> Bool
contains x [] = False
contains x (y:ys) = x == y || contains x ys
{-
error:
    • No instance for (Eq a) arising from a use of ‘==’
      Possible fix:
        add (Eq a) to the context of
          the type signature for:
            contains :: forall a. a -> [a] -> Bool
    • In the first argument of ‘(||)’, namely ‘x == y’
      In the expression: x == y || contains x ys
      In an equation for ‘contains’:
          contains x (y : ys) = x == y || contains x ys
  |
3 | contains x (y:ys) = x == y || contains x ys
  |                     ^^^^^^
-}