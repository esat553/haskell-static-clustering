contains :: a -> [a] -> Bool
contains n [] = False 
contains n (x:xs) 
 | n == x = True
 | otherwise = contains elem xs
 
{-
error:
    • Couldn't match type ‘a’ with ‘a0 -> t0 a0 -> Bool’
      ‘a’ is a rigid type variable bound by
        the type signature for:
          contains :: forall a. a -> [a] -> Bool
        at 1:1-28
      Expected type: [a0 -> t0 a0 -> Bool]
        Actual type: [a]
    • In the second argument of ‘contains’, namely ‘xs’
      In the expression: contains elem xs
      In an equation for ‘contains’:
          contains n (x : xs)
            | n == x = True
            | otherwise = contains elem xs
    • Relevant bindings include
        xs :: [a] (bound at 3:15)
        x :: a (bound at 3:13)
        n :: a (bound at 3:10)
        contains :: a -> [a] -> Bool (bound at 2:1)
  |
5 |  | otherwise = contains elem xs
  |                              ^^
-}