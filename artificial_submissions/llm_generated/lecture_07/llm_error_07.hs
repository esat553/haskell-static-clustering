class Size a where
  size :: a -> Int

instance Size [a] where
  size :: [a] -> Int
  size xs = length xs

{-
no issuein compilation
-}