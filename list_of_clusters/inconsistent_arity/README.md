# Abweichende Arity (inconsistent arity)

## Deutsche Beschreibung
Dieser Cluster fasst Fehler zusammen, die entstehen, wenn eine Funktion mittels Pattern Matching über mehrere Gleichungen (Zeilen) definiert wird und diese Gleichungen eine unterschiedliche Anzahl von Argumenten erwarten. Dadurch ist die Definition der Funktion in sich widersprüchlich und der Compiler meldet `Equations for ‘...’ have different numbers of arguments`.

## Mögliche Behebung (Fix)
Die Definitionszeilen der betroffenen Funktion so anpassen, dass jede Zeile exakt dieselbe Anzahl an Argumenten durch Pattern Matching entgegennimmt. Die Anzahl der Argumente muss dabei mit der Typ-Signatur der Funktion übereinstimmen.

## English Description
This cluster covers errors that arise when a function is defined across multiple equations (lines) using pattern matching, and these equations expect a different number of arguments. This makes the function's definition self-contradictory, causing the compiler to report `Equations for ‘...’ have different numbers of arguments`.

## Suggested Fix
Adjust the definition of the affected function so that every equation accepts the exact same number of arguments via pattern matching. The number of arguments must also match the function's type signature.

## Regex Pattern
```python
equations for .* have different numbers of arguments
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
