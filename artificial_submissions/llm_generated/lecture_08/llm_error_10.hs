analyse :: [String] -> [(Char, Int)]
analyse xs = 
    do 
      s <- xs
      let c = head s
      let firsts = map head xs
      let count = length $ filter (==c) firsts
      return (c, count)

{-
Ground Truth Metadata:
Error Type: Fehlanwendung der do-Notation / Misuse of do-Notation
Short Description: The `do`-notation is used in a non-monadic context to process a list, resulting in a value wrapped in a list monad rather than a plain list of tuples as expected by the function signature.
Intended Root Cause: Student misunderstands the purpose of `do`-notation and applies it outside of a proper monadic context, leading to unintended return types.
Affected Line(s): 3–8
-}
-- RegEx-Cluster: 