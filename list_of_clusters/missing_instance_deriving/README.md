# Fehlende Instanz bei 'deriving' (Missing Instance in 'deriving')

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn beim automatischen Ableiten (`deriving`) einer Instanz eine notwendige Instanz für einen Feldtyp fehlt. Zum Beispiel kann `deriving Show` nur verwendet werden, wenn alle Feldtypen ebenfalls eine `Show`-Instanz besitzen. Der Fehler entsteht beim Kompilieren der `deriving`-Anweisung.

## Mögliche Behebung (Fix)
Stelle sicher, dass alle in der Datenstruktur verwendeten Typen die benötigte Instanz besitzen. Ergänze ggf. ein `deriving` beim Feldtyp oder definiere eine manuelle Instanz.

## English Description
This error occurs when a `deriving` clause fails because one or more field types do not provide the required instance. For example, `deriving Show` only works if all field types also implement `Show`. The error appears during compilation of the `deriving` statement.

## Suggested Fix
Ensure that all types used in the data structure provide the required instance. Add a `deriving` clause to the field type or write a manual instance if necessary.


## Regex Pattern
```python
When deriving the instance for
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
