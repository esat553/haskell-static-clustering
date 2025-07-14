# Typed Hole (Typed Hole)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn im Code ein Platzhalter `_` an einer Stelle verwendet wird, an der ein konkreter Wert erwartet wird. Der Compiler meldet dann ein „typed hole“ und zeigt an, welchen Typ der fehlende Ausdruck haben müsste.

## Mögliche Behebung (Fix)
Ersetze den Unterstrich `_` durch einen passenden Ausdruck mit dem geforderten Typ, wie vom Compiler in der Fehlermeldung angegeben.

## English Description
This error occurs when an underscore `_` is used as a placeholder where a concrete value is expected. The compiler reports a "typed hole" and indicates the type required for that expression.

## Suggested Fix
Replace the underscore `_` with an appropriate expression of the type specified by the compiler in the error message.


## Regex Pattern
```python
found hole: _ ::
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
