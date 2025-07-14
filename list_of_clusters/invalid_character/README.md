# Ungültiges Zeichen (Invalid Character)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn ein Operator oder Funktionsname in einfachen Hochkommata (z. B. `'mod'` oder `'mappend'`) statt wie üblich ohne oder in Backticks verwendet wird. Haskell interpretiert solche Zeichen als ungültig und meldet einen Syntaxfehler.

## Mögliche Behebung (Fix)
Verwende Funktionsnamen ohne Hochkommata, entweder als Präfixnotation (`mod x y`) oder mit Backticks als Infixnotation (``x `mod` y``).

## English Description
This error occurs when an operator or function name is written in single quotes (e.g., `'mod'` or `'mappend'`) instead of the correct syntax. Haskell treats such usage as invalid and raises a syntax error.

## Suggested Fix
Use function names without single quotes, either in prefix form (`mod x y`) or as infix with backticks (``x `mod` y``).


## Regex Pattern
```python
syntax error
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
