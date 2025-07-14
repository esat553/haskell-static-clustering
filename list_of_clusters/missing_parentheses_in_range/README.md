# Fehlende Klammern im Range-Ausdruck (Missing Parentheses in Range Expression)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn ein Aufzählungsbereich (z. B. `Club..Spade` oder `Two..Ace`) ohne Klammern in einer List Comprehension oder ähnlichem Kontext verwendet wird. Haskell verlangt bei Aufzählungstypen die Schreibweise mit runden Klammern: `(Club .. Spade)`.

## Mögliche Behebung (Fix)
Setze den Bereichsausdruck bei Aufzählungstypen immer in Klammern, z. B. `[x | x <- (Club .. Spade)]` statt `[x | x <- [Club..Spade]]`.

## English Description
This error occurs when an enumeration range (e.g., `Club..Spade` or `Two..Ace`) is used without parentheses in a list comprehension or similar context. Haskell requires the use of round brackets for ranges over enumeration types: `(Club .. Spade)`.

## Suggested Fix
Always enclose the range expression for enumeration types in parentheses, e.g., `[x | x <- (Club .. Spade)]` instead of `[x | x <- [Club..Spade]]`.

## Regex Pattern
```python
a section must be enclosed in parentheses
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
