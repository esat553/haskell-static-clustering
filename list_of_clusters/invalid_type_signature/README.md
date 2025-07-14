# Ungültige Typensignatur (Invalid Type Signature)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn eine Typsignatur syntaktisch falsch ist oder an einer nicht erlaubten Stelle steht, zum Beispiel innerhalb einer `instance`-Deklaration.

## Mögliche Behebung (Fix)
Typsignatur korrekt formulieren und nur dort einsetzen, wo sie erlaubt ist.

## English Description
This error occurs when a type signature is syntactically incorrect or used in a disallowed position, such as inside an `instance` declaration.

## Suggested Fix
Write a valid type signature and place it only where allowed. Ensure correct syntax and valid operators.


## Regex Pattern
```python
(invalid|illegal) type signature
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs) and [example_2.hs](./example_2.hs).
