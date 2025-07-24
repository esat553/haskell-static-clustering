# Ungültiger Typ-Operator (Illegal Type Operator)

> deprecated in in ghc > 8.10.2 

## Deutsche Beschreibung
Dieser Fehler entsteht, wenn in einer Typsignatur ein ungültiger Operator verwendet wird. Meist passiert das durch die versehentliche Nutzung falscher Zeichen wie eines typografischen Minuszeichens `−` anstelle des korrekten ASCII-Zeichens `->` oder durch die fälschliche Verwendung von Operatoren wie `+` oder `:` als Typbestandteile.

## Mögliche Behebung (Fix)
Stelle sicher, dass nur gültige Haskell-Syntax verwendet wird – insbesondere das korrekte `->` für Funktionspfeile. Vermeide Operatoren wie `+` oder `:` in Typdefinitionen, wenn sie dort nicht erlaubt sind. Achte auch auf unsichtbare Unicode-Zeichen.

## English Description
This error occurs when an invalid operator is used in a type signature, often due to incorrect characters like a typographic minus `−` instead of the ASCII arrow `->`, or misuse of operators like `+` or `:` within types.

## Suggested Fix
Ensure that only valid Haskell syntax is used – especially `->` for function types. Avoid operators like `+` or `:` in type declarations unless explicitly defined. Watch out for invisible Unicode characters.


## Regex Pattern
```python
illegal operator .* in type .*
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
