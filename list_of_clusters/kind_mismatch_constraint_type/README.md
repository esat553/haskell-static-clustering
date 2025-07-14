# Kind-Konflikt (Constraint vs. Typ) (Kind Mismatch (Constraint vs. Type))

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn eine Typklasse wie `Num a` fälschlich als konkreter Typ verwendet wird. Constraints wie `Num` haben das Kind `* -> Constraint`, dürfen aber nicht direkt als Listentyp `[Num a]` auftreten, da Listen nur konkrete Typen vom Kind `*` enthalten können.

## Mögliche Behebung (Fix)
Verwende stattdessen einen Typparameter mit vorangestellter Constraint: `foo :: Num a => [a] -> [a]`.

## English Description
This error occurs when a type class like `Num a` is wrongly used as a concrete type. Constraints have kind `* -> Constraint` and cannot be used directly in list types like `[Num a]`, which expect kind `*`.

## Suggested Fix
Use a type variable with a constraint: `foo :: Num a => [a] -> [a]`.

## Regex Pattern
```python
expected (a constraint|a type), but .* has kind
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
