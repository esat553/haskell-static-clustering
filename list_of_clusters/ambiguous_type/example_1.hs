stdList = 1 : 2 : 3 : 4 : 5
{-
error:
    • Ambiguous type variable ‘a0’ arising from the literal ‘1’
      prevents the constraint ‘(Num a0)’ from being solved.
      Relevant bindings include
        stdList :: [a0] (bound at Studentenlösung:1:1)
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      These potential instances exist:
        instance Num Integer -- Defined in ‘GHC.Num’
        instance Num Double -- Defined in ‘GHC.Float’
        instance Num Float -- Defined in ‘GHC.Float’
        ...plus two others
        (use -fprint-potential-instances to see them all)
    • In the first argument of ‘(:)’, namely ‘1’
      In the expression: 1 : 2 : 3 : 4 : 5
      In an equation for ‘stdList’: stdList = 1 : 2 : 3 : 4 : 5
  |
1 | stdList = 1 : 2 : 3 : 4 : 5
  |           ^

error:
    • No instance for (Num [a0]) arising from the literal ‘5’
    • In the second argument of ‘(:)’, namely ‘5’
      In the second argument of ‘(:)’, namely ‘4 : 5’
      In the second argument of ‘(:)’, namely ‘3 : 4 : 5’
  |
1 | stdList = 1 : 2 : 3 : 4 : 5
  |                           ^
-}