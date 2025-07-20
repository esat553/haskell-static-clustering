data Box a = Full | Empty

instance Functor Box where
  fmap f Full = Full
  fmap f Empty = Empty

{-
no real error found here
-}