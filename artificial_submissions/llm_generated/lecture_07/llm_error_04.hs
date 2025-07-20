class Size a where
  size :: a -> Int

instance Size (Maybe a) where
  size (Just _) = 1

instance Size Bool where
  size True = 1

{-
Ground Truth Metadata:
Error Type: Nicht alle Fälle abgedeckt / Incomplete Pattern Matching
Short Description: The instance definitions for `Maybe a` and `Bool` do not cover all possible constructors, which can lead to runtime errors due to non-exhaustive pattern matches.
Intended Root Cause: Student assumes that only the relevant or typical cases need to be implemented and overlooks the requirement for total pattern coverage.
Affected Line(s): 4, 7
-}
-- RegEx-Cluster: 