main = do
    putStrLn "nein" main
{-
error:
    • Couldn't match expected type ‘t -> t’ with actual type ‘IO ()’
    • The function ‘putStrLn’ is applied to two arguments,
      but its type ‘String -> IO ()’ has only one
      In a stmt of a 'do' block: putStrLn "nein" main
      In the expression: do putStrLn "nein" main
    • Relevant bindings include main :: t (bound at Studentenlösung:1:1)
  |
2 |     putStrLn "nein" main
  |     ^^^^^^^^^^^^^^^^^^^^
-}