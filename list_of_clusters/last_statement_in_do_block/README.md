# Letzte Anweisung im 'do'-Block (Last Statement in 'do' Block)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn die letzte Anweisung in einem `do`-Block keine gültige Ausdrucksanweisung ist, sondern beispielsweise nur eine `let`-Bindung ohne nachfolgenden Ausdruck. Haskell erwartet am Ende eines `do`-Blocks stets einen ausführbaren Ausdruck.

## Mögliche Behebung (Fix)
Ergänze nach der `let`-Bindung einen gültigen Ausdruck oder eine Aktion, z. B. `return ()` oder eine weitere IO-Anweisung.

## English Description
This error occurs when the last statement in a `do` block is not a valid expression, for example, if it is only a `let` binding without a following expression. Haskell expects a final executable expression at the end of every `do` block.

## Suggested Fix
Add a valid expression or action after the `let` binding, such as `return ()` or another IO command.


## Regex Pattern
```python
the last statement in a 'do' block must be an expression
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
