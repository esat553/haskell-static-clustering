analyse :: [String] -> [(Char, Int)]
analyse xs =
    let firstChars = map head xs
    in map (\s ->
        let c = head s
            count = foldr (\x acc -> if (head x) == c then acc + 1 else acc) 0 firstChars
        in (c, count)
    ) xs

{-
Ground Truth Metadata:
Error Type: Falscher Argumenttyp für `head` / Incorrect Argument Type for `head`
Short Description: The expression `head x` assumes that `x` is a `String`, but `x` is actually a `Char`, since `firstChars` is a list of characters.
Intended Root Cause: Student confuses the types of elements in `firstChars` and incorrectly assumes that each element is a string rather than a character.
Affected Line(s): 6
-}
-- RegEx-Cluster: Typenkonflikt