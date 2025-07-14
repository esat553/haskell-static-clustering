# Fehlendes Binding (Missing Binding)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn eine Typdefinition (Type Signature) angegeben wurde, jedoch keine passende Funktionsdefinition (Binding) folgt. Haskell erwartet nach jeder Signatur auch eine konkrete Implementierung. Fehlt diese, resultiert ein Compilerfehler.

## Mögliche Behebung (Fix)
Es ist sicherzustellen, dass jeder Typdeklaration eine Funktionsdefinition mit identischem Namen folgt. Soll die Funktion zu einem späteren Zeitpunkt implementiert werden, kann als Platzhalter `undefined` verwendet oder die Signatur vorübergehend entfernt werden.

## English Description
This error occurs when a type signature is provided without an accompanying function definition. Haskell expects a concrete binding after each type declaration. If it is missing, a compiler error is raised.

## Suggested Fix
It must be ensured that every type signature is followed by a matching function definition. If the function is to be implemented later, a placeholder such as `undefined` may be used, or the type signature may be temporarily removed.


## Regex Pattern
```python
type signature.*lacks an accompanying binding
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
