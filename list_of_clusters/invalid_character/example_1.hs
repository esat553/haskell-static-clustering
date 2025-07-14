foo x y = x 'mod' y

{-
error:
    • Syntax error on 'mod'
      Perhaps you intended to use TemplateHaskell or TemplateHaskellQuotes
    • In the Template Haskell quotation 'mod'
  |
1 | foo x y = x 'mod' y
  |             ^^^^^
-}