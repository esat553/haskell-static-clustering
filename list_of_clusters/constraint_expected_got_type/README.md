# Constraint erwartet, aber Typ erhalten (Constraint Expected, Got Type)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn im Typzusammenhang eine Typsignatur angegeben wird, bei der ein Typ anstelle einer Typklassen-Constraint steht. In Haskell müssen Constraints (wie `Eq a`, `Show a` oder `Num a`) im Typkopf durch `=>` eingeleitet werden. Wenn versehentlich ein konkreter Typ dort angegeben wird, interpretiert der Compiler dies als Constraint erwartet aber eine Typklasse, nicht einen Datentyp.

## Mögliche Behebung (Fix)
Es ist sicherzustellen, dass im Typkopf nur gültige Typklassen-Constraints stehen.

## English Description
This error occurs when a type is mistakenly written in the place of a type class constraint. In Haskell, constraints (like `Eq a`, `Num a`) must appear before the `=>` arrow.

## Suggested Fix
Ensure that only valid type class constraints appear before the `=>`. 


## Regex Pattern
```python
expected a constraint, but .* has kind
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
