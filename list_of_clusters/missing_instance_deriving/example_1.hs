instance Monoid Integer where
  mempty = 0
  mappend x y = x + y

  {-
  error:
    • No instance for (Semigroup Integer)
        arising from the superclasses of an instance declaration
    • In the instance declaration for ‘Monoid Integer’
  |
1 | instance Monoid Integer where
  |          ^^^^^^^^^^^^^^
  -}