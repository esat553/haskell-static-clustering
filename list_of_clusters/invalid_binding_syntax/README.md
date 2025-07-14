# Ungültige Binding-Syntax (Invalid Binding Syntax)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn beim Definieren eines neuen Typs oder einer Bindung eine Haskell-intern reservierte Syntax, wie Tupel-Konstrukte oder andere spezielle Symbole, irrtümlich als Konstruktor verwendet wird. Bestimmte Syntaxelemente dürfen nicht als eigene Typdefinitionen oder Konstruktoren genutzt werden.

## Mögliche Behebung (Fix)
Verwende für neue Typen und Konstruktoren ausschließlich gültige und selbstgewählte Namen, keine eingebauten Syntaxelemente wie `(,)` für Tupel.

## English Description
This error occurs when built-in Haskell syntax, such as tuple notation or special symbols, is incorrectly used as a constructor or in a type definition. Certain syntactic forms are reserved and cannot be redefined or used in this way.

## Suggested Fix
Use only valid and user-defined names for new types and constructors, and avoid built-in syntax elements like `(,)` for tuples.

## Regex Pattern
```python
illegal binding of built-in syntax
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
