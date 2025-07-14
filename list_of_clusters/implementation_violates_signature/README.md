# Implementierung verletzt Typsignatur (Implementation Violates Type Signature)

## Deutsche Beschreibung
Dieser Cluster fasst Fehler zusammen, bei denen die Implementierung einer Funktion (der Code rechts vom `=`) die eigene Typ-Signatur (der Teil mit `::`) verletzt. Dies geschieht oft im Kontext von Polymorphismus, wenn die Signatur eine allgemeine Funktion verspricht, die Implementierung aber von einem spezifischeren Typ ausgeht.

## Mögliche Behebung (Fix)
Entweder muss die Implementierung so angepasst werden, dass sie zur definierten Typsignatur passt, oder die Signatur muss angepasst werden, sodass sie korrekt beschreibt, was der Code implementiert.

## English Description
This cluster covers errors where a function's implementation (the code to the right of the `=`) violates its own type signature (the part with `::`). This often occurs in the context of polymorphism, when the signature promises a general function, but the implementation assumes a more specific type.

## Suggested Fix
Either the implementation must be adjusted to match the defined type signature, or the signature must be adjusted to correctly describe what the code implements.

## Regex Pattern
```python
is a rigid type variable bound by
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
