{-
Schreiben Sie die folgenden Funktionen im Pointfree-Stil um. Sie durfen jede dieser Funktionen und ¨
Operatoren verwenden: flip, head, tail, (.), ($), (,).

foo x = sqrt (5 * (sum (take x [1..50])))
-}
{-
english translation:
Rewrite the following function in pointfree style.
You may use any of these functions and operators: flip, head, tail, (.), ($), (,).

foo x = sqrt (5 * (sum (take x [1..50])))
-}
foo = sqrt . (5 *) . sum . flip take [1..50]