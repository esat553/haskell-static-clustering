data ConflictingType = A | B deriving (Eq)
data WrapperType = Wrap ConflictingType deriving (Eq)

instance Eq ConflictingType where
    A == A = True
    _ == _ = False

  {-
error:
    • Overlapping instances for Eq ConflictingType
        arising from the first field of ‘Wrap’ (type ‘ConflictingType’)
      Matching instances:
        instance Eq ConflictingType -- Defined at 1:40
        instance Eq ConflictingType -- Defined at 4:10
    • When deriving the instance for (Eq WrapperType)
  |
2 | data WrapperType = Wrap ConflictingType deriving (Eq)
  |                                                   ^^
  -}