# Kind-Konflikt (Kind Mismatch)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn ein Typ an einer Stelle verwendet wird, an der ein anderer Kind-Ausdruck erwartet wird. Ein häufiger Auslöser ist die fälschliche Verwendung eines konkreten Datentyps (Kind `*`) als Typklasse oder Constraint (Kind `* -> Constraint`).

## Mögliche Behebung (Fix)
Entferne ungültige Typklassen-Constraints wie `Term a =>`, sofern der betroffene Typ kein Typkonstruktor oder keine Typklasse ist. Überprüfe die Typsignatur auf Kind-Konsistenz zwischen erwarteten und verwendeten Typen.

## English Description
This error occurs when a type is used in a position where a different kind is expected. A common cause is the incorrect use of a concrete data type (kind `*`) as a type class or constraint (kind `* -> Constraint`), leading to a mismatch in the type signature structure.

## Suggested Fix
Remove invalid type class constraints such as `Term a =>` if the corresponding type is not a type constructor or class. Ensure that all parts of the type signature are kind-consistent.


## Regex Pattern
```python
expected kind .* but .* has kind
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
