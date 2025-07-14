# Mehrdeutiger Bezeichner (Ambiguous Identifier)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn im Gültigkeitsbereich (Scope) mehrere Definitionen für denselben Namen existieren und der Compiler nicht eindeutig entscheiden kann, welche gemeint ist. Dies geschieht typischerweise, wenn ein Name aus dem `Prelude` oder einem importierten Modul verwendet wird und gleichzeitig eine eigene Definition mit demselben Namen im Code existiert.

## Mögliche Behebung (Fix)
Die Behebung besteht darin, die Mehrdeutigkeit aufzulösen, indem entweder der lokale Name umbenannt wird oder durch Qualifizierung eindeutig gemacht wird (z. B. `Prelude.compare` statt `compare`). Alternativ sollte auf die eigene Definition verzichtet werden, wenn sie mit einer Standardfunktion kollidiert.

## English Description
This error occurs when multiple definitions for the same name are in scope, and the compiler cannot unambiguously determine which one is intended. This typically happens when a name from the `Prelude` or an imported module is reused in the local code. 

## Suggested Fix
To fix this error, disambiguate the conflicting name by either renaming the local definition or qualifying the standard function explicitly (e.g., `Prelude.compare` instead of `compare`). Alternatively, avoid redefining standard functions or types that already exist in the `Prelude`. Choosing unique names and avoiding shadowing known definitions will prevent this issue.


## Regex Pattern
```python
ambiguous occurrence
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
