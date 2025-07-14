isCrewmate :: Astronaut -> Bool
isCrewmate a = role a == Crewmate

{-
error:
    Not in scope: type constructor or class ‘Astronaut’
  |
1 | isCrewmate :: Astronaut -> Bool
  |               ^^^^^^^^^
-}