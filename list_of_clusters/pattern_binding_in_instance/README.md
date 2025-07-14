# Pattern Binding in Instanz (Pattern Binding in Instance Declaration)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn innerhalb einer `instance`-Deklaration ein Pattern-Match anstelle einer Funktionsdefinition mit Parametern verwendet wird.

## Mögliche Behebung (Fix)
Statt eines Pattern-Bindings muss eine reguläre Funktionsdefinition mit benannten Parametern verwendet werden.

## English Description
This error occurs when a pattern match is used directly within an `instance` declaration. Haskell does not allow pattern bindings in instance declarations.

## Suggested Fix
Replace the pattern binding with a proper function definition using parameters

## Regex
```python
pattern bindings.*not allowed in instance declaration
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
