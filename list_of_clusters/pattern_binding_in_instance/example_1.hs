data Triple a = Triple a a a

instance Show (Triple a) where
  Show (Triple x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

{-
error:
    Pattern bindings (except simple variables) not allowed in instance declaration:
      Show (Triple x y z)
        = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"
  |
4 |   Show (Triple x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"
  |   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}