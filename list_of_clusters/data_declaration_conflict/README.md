# Konflikt in 'data'-Deklaration (Conflict in 'data' Declaration)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn in einer `data`-Deklaration derselbe Typname mehrfach als Typparameter verwendet wird. In Haskell muss jeder Typparameter in einer Datentypdefinition eindeutig sein.

## Mögliche Behebung (Fix)
Vergib für jeden Typparameter in der `data`-Deklaration einen eigenen, eindeutigen Namen.

## English Description
This error occurs when the same type name is used more than once as a type parameter in a `data` declaration. In Haskell, each type parameter in a data type definition must be unique.

## Suggested Fix
Assign a unique name to each type parameter in the `data` declaration.


## Regex Pattern
```python
Conflicting definitions for\s+['‘`]?.+?['’`]?\s+.*?\n\s*\|\s*\n\s*\d+\s*\|\s*data
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
