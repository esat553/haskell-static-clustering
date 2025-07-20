analyse :: [String] -> [(Char, Int)]
analyse xs = 
    let firstChars = map head xs
    in map (\s -> 
        let c = head s
            count = foldr (\x acc -> if x == c then acc + 1 else acc) 'a' firstChars
        in (c, count)
    ) xs

{-
Ground Truth Metadata:
Error Type: Falscher Startwert im Fold / Incorrect Initial Value in Fold
Short Description: The initial value for the `foldr` is a `Char` ('a') instead of an `Int`, which causes a type mismatch when performing arithmetic operations like `+ 1`.
Intended Root Cause: Student confuses character literals with numeric types and mistakenly initializes the accumulator with a `Char`.
Affected Line(s): 6
-}
-- RegEx-Cluster: Typenkonflikt