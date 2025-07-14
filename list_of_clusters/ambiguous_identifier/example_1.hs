--6.3.hs
all :: (a -> Bool) -> [a] -> Bool
all f (x:xs) = f x && all xs
all f [] = True

{-
error:
    Ambiguous occurrence ‘all’
    It could refer to either ‘Prelude.all’,
                             imported from ‘Prelude’ at Studentenlösung:1:1
                             (and originally defined in ‘Data.Foldable’)
                          or ‘Main.all’, defined at Studentenlösung:3:1
  |
3 | all f (x:xs) = f x && all xs
  |                       ^^^
-}