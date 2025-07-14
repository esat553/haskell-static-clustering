data Triple a a a = Triple a a a
  deriving (Eq)
  {-
  Studentenlösung:1:13-17: error:
    Conflicting definitions for ‘a’
    Bound at: Studentenlösung:1:13
              Studentenlösung:1:15
              Studentenlösung:1:17
  |
1 | data Triple a a a = Triple a a a
  |             ^^^^^
  -}