ungerade :: Integral a => a -> Bool
ungerade = do
    if a `mod` 2 == 0
        then return False 
        else return True 

{-
error: Variable not in scope: a :: Integer
  |
3 |     if a `mod` 2 == 0
  |        ^
-}