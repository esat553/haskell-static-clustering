alleUnterschied :: Eq a => a -> a -> a -> Bool
alleUnterschied (a, b, c) = (a == b) && (b == c)

{-
error:
    • Couldn't match expected type ‘a -> a -> Bool’
                  with actual type ‘Bool’
    • Possible cause: ‘(&&)’ is applied to too many arguments
      In the expression: (a == b) && (b == c)
      In an equation for ‘alleUnterschied’:
          alleUnterschied (a, b, c) = (a == b) && (b == c)
    • Relevant bindings include
        alleUnterschied :: a -> a -> a -> Bool (bound at Studentenlösung:2:1)
  |
2 | alleUnterschied (a, b, c) = (a == b) && (b == c)
  |                             ^^^^^^^^^^^^^^^^^^^^
-}