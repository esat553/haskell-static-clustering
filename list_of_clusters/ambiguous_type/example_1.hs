-- Ambigous.hs
module Main where

main :: IO ()
main = do
  let ambiguousValue = read "5"
  print ambiguousValue
{-
error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘read’
      prevents the constraint ‘(Read a0)’ from being solved.
      Relevant bindings include
        ambiguousValue :: a0 (bound at Studentenlösung:6:7)
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      Potentially matching instances:
        instance Read Ordering -- Defined in ‘GHC.Read’
        instance Read a => Read (Maybe a) -- Defined in ‘GHC.Read’
        ...plus 24 others
        ...plus 10 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In the expression: read "5"
      In an equation for ‘ambiguousValue’: ambiguousValue = read "5"
      In the expression:
        do let ambiguousValue = read "5"
           print ambiguousValue
  |
6 |   let ambiguousValue = read "5"
  |                        ^^^^

error:
    • Ambiguous type variable ‘a0’ arising from a use of ‘print’
      prevents the constraint ‘(Show a0)’ from being solved.
      Relevant bindings include
        ambiguousValue :: a0 (bound at Studentenlösung:6:7)
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      Potentially matching instances:
        instance Show Ordering -- Defined in ‘GHC.Show’
        instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
        ...plus 25 others
        ...plus 12 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In a stmt of a 'do' block: print ambiguousValue
      In the expression:
        do let ambiguousValue = read "5"
           print ambiguousValue
      In an equation for ‘main’:
          main
            = do let ambiguousValue = ...
                 print ambiguousValue
  |
7 |   print ambiguousValue
  |   ^^^^^
-}