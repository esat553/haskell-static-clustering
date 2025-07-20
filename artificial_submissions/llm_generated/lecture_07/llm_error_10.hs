class Size a where
  size :: a -> Int

instance Size Bool where

size True = 1
size False = 1

{-
Ground Truth Metadata:
Error Type: Funktionsdefinition außerhalb der Instanz / Function Definition Outside of Instance Block
Short Description: The function `size` is defined outside the `instance` block, making it a top-level definition rather than part of the type class implementation.
Intended Root Cause: Student does not realize that type class methods must be implemented within the corresponding `instance` block and assumes they can be defined globally.
Affected Line(s): 6–7
-}
-- RegEx-Cluster: Mehrfache Deklarationen