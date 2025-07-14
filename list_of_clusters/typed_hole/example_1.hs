foo :: Int
foo = _

{-
error:
    • Found hole: _ :: Int
    • In the expression: _
      In an equation for ‘foo’: foo = _
    • Relevant bindings include foo :: Int (bound at Studentenlösung:2:1)
      Valid substitutions include
        foo :: Int (defined at Studentenlösung:2:1)
        undefined :: forall (a :: TYPE r).
                     GHC.Stack.Types.HasCallStack =>
                     a
          (imported from ‘Prelude’ at Studentenlösung:1:1
           (and originally defined in ‘GHC.Err’))
        maxBound :: forall a. Bounded a => a
          (imported from ‘Prelude’ at Studentenlösung:1:1
           (and originally defined in ‘GHC.Enum’))
        minBound :: forall a. Bounded a => a
          (imported from ‘Prelude’ at Studentenlösung:1:1
           (and originally defined in ‘GHC.Enum’))
  |
2 | foo = _
  |       ^
-}