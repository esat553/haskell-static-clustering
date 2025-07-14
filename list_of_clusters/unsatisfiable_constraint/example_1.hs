data Term a = C a | Division (Term a) (Term a)

eval :: Fractional a => Term a -> a
eval (Division x y) = div (eval x) (eval y)

{-
error:
    • Could not deduce (Integral a) arising from a use of ‘div’
      from the context: Fractional a
        bound by the type signature for:
                   eval :: forall a. Fractional a => Term a -> a
        at Studentenlösung:3:1-35
      Possible fix:
        add (Integral a) to the context of
          the type signature for:
            eval :: forall a. Fractional a => Term a -> a
    • In the expression: div (eval x) (eval y)
      In an equation for ‘eval’:
          eval (Division x y) = div (eval x) (eval y)
  |
4 | eval (Division x y) = div (eval x) (eval y)
  |                       ^^^^^^^^^^^^^^^^^^^^^
-}