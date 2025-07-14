# Numerischer Typenkonflikt (Numeric Type Conflict)

## Deutsche Beschreibung
Dieser Cluster behandelt einen der häufig vorkommenden Fehler bei Studenten: Konflikte zwischen verschiedenen numerischen Typen. Haskell unterscheidet streng zwischen Ganzzahlen (z.B. `Int`) und Gleitkommazahlen (z.B. `Double`), die nicht direkt vermischt werden dürfen. Der Fehler entsteht oft bei der Division, wenn der Operator `/` fälschlicherweise für Ganzzahlen verwendet wird, oder wenn eine Funktion einen bestimmten numerischen Typ erwartet, aber einen anderen erhält.

## Mögliche Behebung (Fix)
Für die Division von Ganzzahlen muss der Operator `div` anstelle von `/` verwendet werden. Um Ganzzahlen in Gleitkommazahlen umzuwandeln und so Typen zu mischen, muss die Funktion `fromIntegral` benutzt werden (z.B. `(fromIntegral 3) / 2.0`).

## English Description
This cluster covers one of the most common errors in Haskell: conflicts between different numeric types. Haskell strictly distinguishes between whole numbers (e.g., `Int`) and floating-point numbers (e.g., `Double`), which cannot be mixed directly. The error often arises from using the `/` operator incorrectly for integer division, or when a function expects a specific numeric type but receives another.

## Suggested Fix
For the division of whole numbers, use the `div` operator instead of `/`. To mix types by converting integers to floating-point numbers, use the `fromIntegral` function (e.g., `(fromIntegral 3) / 2.0`).

## Regex Pattern
```python
No instance for \(Fractional\s+[A-Za-z0-9]+\)|No instance for \(Num\s+[A-Za-z0-9]+\)|Couldn't match expected type\s+‘?(Double|Float|Rational|Int|Integer|Num\s+[a-zA-Z0-9]*)’?\s+with actual type\s+‘?(Double|Float|Rational|Int|Integer|Num\s+[a-zA-Z0-9]*)’?
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
