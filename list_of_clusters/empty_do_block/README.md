# Leerer do-Block (Empty 'do' Block)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn ein `do`-Block deklariert wird, aber keine Anweisungen enthält. Haskell erwartet mindestens einen Ausdruck oder Befehl im `do`-Block.

## Mögliche Behebung (Fix)
Füge mindestens eine gültige Anweisung in den `do`-Block ein, etwa `return ()` oder eine andere passende IO-Operation.

## English Description
This error occurs when a `do` block is declared but contains no statements. Haskell requires at least one expression or command inside the `do` block.

## Suggested Fix
Insert at least one valid statement into the `do` block, such as `return ()` or another appropriate IO action.


## Regex Pattern
```python
empty\s+'do'\s+block
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
