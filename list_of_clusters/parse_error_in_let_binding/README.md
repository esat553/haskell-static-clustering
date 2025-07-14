# Parse-Fehler in 'let'-Binding (Parse Error in 'let' Binding)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn ein Schüler fälschlicherweise ein `let`-Statement zur Deklaration von Variablen außerhalb eines gültigen Kontexts verwendet. In Haskell ist ein `let` nur innerhalb eines `do`-Blocks oder in Kombination mit `in` erlaubt. Wird `let` alleinstehend am Top-Level oder in ungültiger Einrückung verwendet, führt das zu einem Parse-Fehler.

## Mögliche Behebung (Fix)
Verwende `let` nur innerhalb eines `do`-Blocks oder zusammen mit `in`, z. B. `let x = 3 in x + 1`. Für top-level Deklarationen verwende stattdessen normale Funktions- oder Wertdefinitionen ohne `let`.

## English Description
This cluster covers parse errors that occur when students incorrectly use a `let` statement to declare variables. In Haskell, `let` is only allowed inside a `do` block or when followed by an `in` clause. Using `let` on its own, especially at the top level or with incorrect indentation, causes a parse error.

## Suggested Fix
Use `let` only inside a `do` block or in combination with `in`, e.g. `let x = 3 in x + 1`. For top-level definitions, use regular function or value declarations without `let`.

## Regex
```python
parse\s+error\s*\(possibly\s+incorrect\s+indentation[^\)]*\)[\s\S]*?\n\s*\d+\s*\|\s+.*\blet\b[^\n]*=
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
