data Suit = Club | Spade deriving (Enum, Show)

allSuits = [s | s <- [Club..Spade]]

{-
error:
    Not in scope: ‘Club..’
    No module named ‘Club’ is imported.
  |
3 | allSuits = [s | s <- [Club..Spade]]
  |                       ^^^^^^
error:
    A section must be enclosed in parentheses thus: (Club.. Spade)
  |
3 | allSuits = [s | s <- [Club..Spade]]
  |                       ^^^^^^^^^^^
-}