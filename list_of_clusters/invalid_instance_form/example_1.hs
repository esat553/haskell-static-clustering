instance Monoid op where
  mempty = 0
  mappend 0 x = x
  mappend x 0 = x

  {-
  error:
    • Illegal instance declaration for ‘Monoid op’
        (All instance types must be of the form (T a1 ... an)
         where a1 ... an are *distinct type variables*,
         and each type variable appears at most once in the instance head.
         Use FlexibleInstances if you want to disable this.)
    • In the instance declaration for ‘Monoid op’
  |
1 | instance Monoid op where
  |          ^^^^^^^^^
  -}