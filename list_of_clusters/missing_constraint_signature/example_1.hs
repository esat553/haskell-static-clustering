exampleFunction :: a -> a -> Bool
exampleFunction a b = a == b

{-
error:
    • No instance for (Eq a) arising from a use of ‘==’
      Possible fix:
        add (Eq a) to the context of
          the type signature for:
            exampleFunction :: forall a. a -> a -> Bool
    • In the expression: a == b
      In an equation for ‘exampleFunction’: exampleFunction a b = a == b
  |
2 | exampleFunction a b = a == b
  |                       ^^^^^^
-}