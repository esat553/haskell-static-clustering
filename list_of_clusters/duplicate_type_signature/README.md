# Doppelte Signatur (Duplicate Type Signature)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn für denselben Namen (eine Funktion oder eine Konstante) mehr als eine Typ-Signatur im selben Gültigkeitsbereich deklariert wird. Haskell erlaubt für jeden Namen nur eine einzige Signatur, um Eindeutigkeit zu gewährleisten. Dies gilt sowohl für Top-Level-Definitionen als auch für lokale Definitionen in `let`- oder `where`-Blöcken.

## Mögliche Behebung (Fix)
Entferne die überflüssige(n) Typ-Signatur(en), sodass für jeden Namen nur noch eine einzige übrig bleibt.

## English Description
This error occurs when more than one type signature is declared for the same name (a function or a constant) within the same scope. Haskell allows only a single signature per name to ensure clarity. This applies to both top-level definitions and local definitions within `let` or `where` clauses.

## Suggested Fix
Remove the redundant type signature(s), leaving only one for each name.

## Regex Pattern
```python
duplicate type signatures?
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs) or [example_2.hs](./example_2.hs).
