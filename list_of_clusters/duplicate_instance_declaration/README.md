# Mehrfache Instanzdeklaration (Duplicate Instance Declaration)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn für denselben Typ und dieselbe Typklasse mehr als eine Instanz definiert wurde. Dies ist in Haskell nicht erlaubt, auch nicht in getrennten Modulen oder über `deriving` und eine manuelle Instanz gleichzeitig.

## Mögliche Behebung (Fix)
Stelle sicher, dass für jeden Typ-Typklassen-Kombination nur eine Instanz vorhanden ist. Entferne doppelte Deklarationen oder verwende nur `deriving` oder eine manuelle Instanz nicht beides.

## English Description
This error occurs when more than one instance is defined for the same type and type class. Haskell does not allow multiple instances for the same combination, even across modules or via both `deriving` and manual implementation.

## Suggested Fix
Ensure that only one instance exists per type-class-type combination. Remove duplicate declarations and choose either `deriving` or a manual instance—but not both.


## Regex Pattern
```python
duplicate instance declarations
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
