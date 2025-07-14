instance Eq Maybe where
  (==) _ _ = True

{-
error:
    • Expecting one more argument to ‘Maybe’
      Expected a type, but ‘Maybe’ has kind ‘* -> *’
    • In the first argument of ‘Eq’, namely ‘Maybe’
      In the instance declaration for ‘Eq Maybe’
  |
1 | instance Eq Maybe where
  |             ^^^^^
-}