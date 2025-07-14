seperateWords :: [String] -> [(String, Int,)]
seperateWords xs = [ (x, length(x), | x <- xs ]

{-
error: parse error on input ‘)’
  |
1 | seperateWords :: [String] -> [(String, Int,)]
  |                                            ^
-}