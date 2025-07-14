main = do
  let x = 3

{-
error:
    The last statement in a 'do' block must be an expression let x = 3
  |
2 |   let x = 3
  |   ^^^^^^^^^
-}