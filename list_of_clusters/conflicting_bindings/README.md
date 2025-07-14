# Konfliktierende Bindings (Conflicting Bindings)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn im selben Gültigkeitsbereich mehrere Bindins mit identischem Namen verwendet werden. Dies kann bei mehrfacher Verwendung desselben Typparameters, Variablennamens oder Funktionsnamens auftreten. Der Compiler meldet einen Konflikt, weil er nicht eindeutig entscheiden kann, auf welche Definition sich ein Verweis bezieht.

## Mögliche Behebung (Fix)
Verwende in Definitionen konsistente und eindeutige Namen. Achte besonders bei Typparametern und Mustern auf doppelte Bindungen und korrigiere sie durch Umbenennung oder Umstrukturierung.

## English Description
This error occurs when multiple bindings with the same name are declared in the same scope. This can happen with repeated type parameters, variables, or function names. The compiler reports a conflict because it cannot resolve which definition to use.

## Suggested Fix
Ensure all names in definitions are unique within their scope. Check for repeated type variables or pattern bindings and resolve them by renaming or restructuring.


## Regex Pattern
```python
conflicting definitions for
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
