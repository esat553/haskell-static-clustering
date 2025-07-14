data Foo a where
  Bar :: Int -> Foo Int

{-
error:
    • Illegal generalised algebraic data declaration for ‘Foo’
        (Enable the GADTs extension to allow this)
    • In the data declaration for ‘Foo’
  |
1 | data Foo a where
  | ^^^^^^^^^^^^^^^^...
-}