take' z [] = []
take' z (x:xs)  | z <= 0 = []
                | otherwise = x :                           (take' (z - 1) xs)

Sorry for the strange spaces, I couldn't change that
-- tail-recursive
take'' z xs = tk z xs []
    where tk z [] acc = acc
          tk z (x:xs) acc   | z <= 0 = acc
                            | otherwise = tk (z - 1) xs (acc ++ [x])
    
{-
Ground Truth Metadata:
Error Type: Natürliche Sprache im Code / Natural Language in Code
Short Description: A natural language sentence ("Sorry for the strange spaces, I couldn't change that") appears in the code section without being marked as a comment.
Intended Root Cause: Student inserts an explanatory text directly into the code, forgetting to mark it as a comment.
Error Class: Knowledge Gap (nach Korkmaz et al. 2015)
Affected Line(s): 5
-}
-- RegEx-Cluster: Parse-Fehler