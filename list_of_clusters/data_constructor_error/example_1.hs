data Card = { suit :: String, value :: Int }
{-
error:
    Cannot parse data constructor in a data/newtype declaration: {suit :: String,
                                                                  value :: Int}
  |
1 | data Card = { suit :: String, value :: Int }
  |             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}