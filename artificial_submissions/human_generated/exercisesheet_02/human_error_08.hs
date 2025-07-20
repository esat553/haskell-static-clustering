rest‘ :: String -> String
rest‘ (_:xs) = xs

{-
Ground Truth Metadata:
Error Type: Unerlaubtes Zeichen / Unpermitted Character
Short Description: An incorrect apostrophe character (‘) is used in the function name instead of the standard ASCII single quote (').
Intended Root Cause: Student uses a typographically similar but syntactically invalid character, possibly due to copying, auto-correction, or keyboard layout.
Error Class: Mental Typo (nach Korkmaz et al. 2015)
Affected Line(s): 1, 2
-}
-- RegEx-Cluster: Lexikalischer Fehler