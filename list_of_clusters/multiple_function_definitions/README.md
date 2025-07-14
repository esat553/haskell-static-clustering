# Mehrfachdefinition in Funktionsgleichung (Multiple Definitions in Function Equation)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn in einer Funktionsgleichung derselbe Variablenname mehrfach auf der linken Seite verwendet wird. In Haskell darf jeder Parametername in einer Funktionsdefinition nur einmal vorkommen, um Mehrdeutigkeiten zu vermeiden.

## Mögliche Behebung (Fix)
Verwende in jedem Pattern einer Funktionsdefinition unterschiedliche Namen für die Argumente und verzichte auf doppelte Benennung.

## English Description
This error occurs when the same variable name is used more than once in the left-hand side of a function equation. In Haskell, each parameter name must appear only once in a pattern to avoid ambiguity.

## Suggested Fix
Use distinct names for each argument in every function pattern and avoid reusing variable names.


## Regex Pattern
```python
conflicting definitions for.*in an equation for
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
