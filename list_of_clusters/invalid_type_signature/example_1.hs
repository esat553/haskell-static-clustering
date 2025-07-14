data Triple a = Triple a a a
    deriving Eq

instance (Show x) => Show (Triple x) where
    show :: Show x => Triple x -> String
    show (Triple a b c) ="(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"
{-
error:
    • Illegal type signature in instance declaration:
        show :: Show x => Triple x -> String
      (Use InstanceSigs to allow this)
    • In the instance declaration for ‘Show (Triple x)’
  |
5 |     show :: Show x => Triple x -> String
  |             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}