# Inkonsistenter Rückgabetyp (Inconsistent Return Type)

## Deutsche Beschreibung
Dieser Cluster behandelt Fehler, bei denen verschiedene Zweige einer Funktion (z.B. in `if/else`- oder `case`-Ausdrücken) Werte mit unterschiedlichen Typen zurückgeben. Laut Typ-Signatur muss eine Funktion jedoch immer einen einzigen, konsistenten Rückgabetyp haben. Der Compiler meldet diesen Widerspruch als `Couldn't match type ...`.

## Mögliche Behebung (Fix)
Sicherstellen, dass alle möglichen Kontrollflüsse einer Funktion (jeder Zweig einer `if`- oder `case`-Anweisung) einen Wert desselben Typs zurückgeben, der mit der Typ-Signatur der Funktion übereinstimmt. Bei `IO ()` statt `return` die passende `putStrLn`- oder `print`-Funktion verwenden. Nach einem `try`-Aufruf müssen die Ergebnisse aus dem `Either`-Container in beiden Zweigen konsistent behandelt werden.

## English Description
This cluster covers errors where different branches of a function (e.g., in `if/else` or `case` expressions) return values of different types. According to its type signature, a function must always have a single, consistent return type. The compiler reports this contradiction as `Couldn't match type ...`.

## Suggested Fix
Ensure that all possible control flows of a function (every branch of an `if` or `case` statement) return a value of the same type, consistent with the function's type signature. For `IO ()`, use the appropriate `putStrLn` or `print` function instead of `return`. After a `try` call, the results from the `Either` container must be handled consistently in both branches.

## Regex Pattern
```python
Couldn't match type .* with .*.*In a case alternative
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
