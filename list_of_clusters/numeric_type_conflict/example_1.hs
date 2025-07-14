pi_approx' :: Int -> Double
pi_approx' x = sqrt (6 * (sum x))
               where sum 1 = 1
                     sum n = 1/(n*n) + sum(n-1)

{-
error:
    • Couldn't match expected type ‘Double’ with actual type ‘Int’
    • In the expression: sqrt (6 * (sum x))
      In an equation for ‘pi_approx'’:
          pi_approx' x
            = sqrt (6 * (sum x))
            where
                sum 1 = 1
                sum n = 1 / (n * n) + sum (n - 1)
  |
8 | pi_approx' x = sqrt (6 * (sum x))
  |                ^^^^^^^^^^^^^^^^^^
-}