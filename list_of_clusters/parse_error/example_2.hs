ungerade :: Integral a => a -> Bool
ungerade  x = `mod` x 2 /= 0

{-
error: parse error on input ‘`’
  |
2 | ungerade  x = `mod` x 2 /= 0
  |               ^
-}