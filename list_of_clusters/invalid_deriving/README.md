# Ungültiges Deriving (Invalid Deriving)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn beim automatischen Ableiten (`deriving`) einer Typklasse ein ungültiger oder falsch geschriebener Typklassenname angegeben wird. Beispielsweise muss `Show` großgeschrieben werden, während `show` als Funktionsname nicht zulässig ist.

## Mögliche Behebung (Fix)
Überprüfe die Schreibweise der abgeleiteten Typklassen im `deriving`-Abschnitt und verwende nur gültige Typklassen wie `Show`, `Eq` oder `Ord` mit korrekter Großschreibung.

## English Description
This error occurs when an invalid or incorrectly written type class name is specified in a `deriving` clause. For example, `Show` must be capitalized, while using the function name `show` is not allowed.

## Suggested Fix
Check the spelling and capitalization of type classes in the `deriving` clause and only use valid type classes like `Show`, `Eq`, or `Ord`.


## Regex Pattern
```python
illegal deriving item
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
