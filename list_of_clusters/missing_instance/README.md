# Fehlende Instanz (Missing Instance)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn eine Funktion eine bestimmte Typklasseninstanz benötigt (z. B. `Eq`, `Ord`, `Show`, `Fractional`), aber keine solche Instanz für den verwendeten Typ existiert. Der Compiler kann die Operation daher nicht ausführen.

## Mögliche Behebung (Fix)
Füge eine passende Instanz mit `deriving` oder per Hand hinzu, oder verwende einen Typ, für den bereits eine solche Instanz existiert.

## English Description
This error occurs when a function requires a type class instance (e.g. `Eq`, `Ord`, `Show`, `Fractional`) that is missing for the given type. The compiler cannot perform the operation without it.

## Suggested Fix
Add the required instance using `deriving` or manual declaration, or use a type that already supports the needed instance.


## Regex Pattern
```python
no instance for
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
