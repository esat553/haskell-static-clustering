data RGB = RGB (Integer, Integer, Integer) deriving Show

addRGB (RGB a b c) (RGB d e f) = RGB (a+b) (b+e) (c+f)

{-
 error:
    • The constructor ‘RGB’ should have 1 argument, but has been given 3
    • In the pattern: RGB a b c
      In an equation for ‘addRGB’:
          addRGB (RGB a b c) (RGB d e f) = RGB (a + b) (b + e) (c + f)
  |
3 | addRGB (RGB a b c) (RGB d e f) = RGB (a+b) (b+e) (c+f)
  |         ^^^^^^^^^
-}