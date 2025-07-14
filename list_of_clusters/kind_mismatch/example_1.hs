data Term = C Integer

eval :: Term a => a
eval = undefined

{-
error:
    • Expected kind ‘* -> Constraint’, but ‘Term’ has kind ‘*’
    • In the type signature: eval :: Term a => a
  |
3 | eval :: Term a => a
  |         ^^^^^^
-}