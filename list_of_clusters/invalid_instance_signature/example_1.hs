
data Box a = Full a

instance Functor Box where
  fmap :: (a -> b) -> Box a -> Box b 
  fmap f (Full x) = Full (f x)

{-
error:
    • Illegal type signature in instance declaration:
        fmap :: (a -> b) -> Box a -> Box b
      (Use InstanceSigs to allow this)
    • In the instance declaration for ‘Functor Box’
  |
5 |   fmap :: (a -> b) -> Box a -> Box b 
  |           ^^^^^^^^^^^^^^^^^^^^^^^^^^
-}