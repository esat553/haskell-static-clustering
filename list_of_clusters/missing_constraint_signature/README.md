# Fehlende Constraint bei Funktionssignatur (Missing Constraint in Function Signature)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn eine Funktion in ihrer Signatur keine Typklasse einschränkt, aber in ihrer Definition Operationen verwendet, die eine solche Einschränkung erfordern.

## Mögliche Behebung (Fix)
Ergänze die passende Typklassen-Constraint in der Signatur.

## English Description
This error occurs when a function signature lacks a type class constraint, even though the function body uses operations that require.

## Suggested Fix
Add the appropriate type class constraint to the function signature. Always match your constraints to the operators or functions you are using.


## Regex Pattern
```python
No instance for \(\w+ [a-z]\) arising from a use of
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
