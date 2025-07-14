myAppend (E) (L b rest) = L b rest 
myAppend (L a rest) (l) = L a (myAppend rest l)
{-
error: Not in scope: data constructor ‘E’
  |
1 | myAppend (E) (L b rest) = L b rest 
  |           ^

error: Not in scope: data constructor ‘L’
  |
1 | myAppend (E) (L b rest) = L b rest 
  |               ^

error: Not in scope: data constructor ‘L’
  |
2 | myAppend (L a rest) (l) = L a (myAppend rest l)  
  |           ^
-}