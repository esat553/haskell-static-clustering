# Falsche Funktionsarität (Incorrect Function Arity)

## Deutsche Beschreibung

Dieser Cluster fasst Fehler zusammen, bei denen eine Funktion oder ein Konstruktor mit einer falschen Anzahl von Argumenten aufgerufen wird. Der Fehler tritt auch auf, wenn die Definition einer Funktion nicht mit ihrer Typ-Signatur übereinstimmt, z. B. wenn eine Funktion laut Signatur drei Argumente erwartet, aber in der Implementierung durch Pattern Matching nur ein einziges Tupel entgegennimmt.

## Mögliche Behebung (Fix)

Überprüfe die Definition und alle Aufrufe der im Fehler genannten Funktion. Stelle sicher, dass die Anzahl der Argumente sowohl in der Definition als auch bei jedem Aufruf exakt mit der Typ-Signatur übereinstimmt.

## English Description
This cluster covers errors where a function or constructor is **called** with the wrong number of arguments. The error also occurs if a function's **definition** does not match its type signature, e.g., when a function expects three arguments according to its signature but is defined to accept a single tuple via pattern matching.

## Suggested Fix
Check the definition and all call sites of the function mentioned in the error. Ensure the number of arguments in both the definition and at every call site exactly matches the type signature.

## Regex
```python
\bapplied to too (few|many) arguments\b|\bhas \w+ arguments, but its type .*? has only \w+
```

For a complete student example including the original error message for both cases, see [example_1.hs](./example_1.hs) and [example_2.hs](./example_2.hs)
