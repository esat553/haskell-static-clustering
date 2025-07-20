data Box a = Full a | Empty

fmap :: (a -> b) -> Box a -> Box b
fmap f (Full x) = Full (f x)
fmap _ Empty    = Empty

instance Functor Box where

{- cant identifiy comile error-}