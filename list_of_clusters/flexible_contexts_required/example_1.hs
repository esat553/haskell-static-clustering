foo :: Eq (Maybe a) => Maybe a -> Bool
foo x = x == Nothing

{-
error:
    • Non type-variable argument in the constraint: Eq (Maybe a)
      (Use FlexibleContexts to permit this)
    • In the type signature: foo :: Eq (Maybe a) => Maybe a -> Bool
  |
1 | foo :: Eq (Maybe a) => Maybe a -> Bool
  |        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}