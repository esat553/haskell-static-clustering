# Funktion nicht definiert (Function Not Defined)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn eine Funktion im Code verwendet wird, ohne dass sie im aktuellen Modul definiert oder importiert wurde. Häufig handelt es sich um Tippfehler, vergessene Definitionen oder fehlende Importe.

## Mögliche Behebung (Fix)
Überprüfe, ob die Funktion korrekt geschrieben, selbst definiert oder das entsprechende Modul importiert wurde. Bei Standardfunktionen wie muss das passende Modul eingebunden werden. Nicht vorhandene eigene Funktionen müssen ergänzt oder korrekt benannt werden.

## English Description
This error occurs when a function is used in the code but is not defined or imported in the current module. It is often caused by typos, missing definitions, or missing imports of standard modules.

## Suggested Fix
Check whether the function is spelled correctly, defined in the code, or properly imported from a module. For standard functions, make sure the appropriate module is imported. Any missing custom functions must be implemented or correctly referenced.


## Regex Pattern
```python
Variable not in scope: ([a-zA-Z_][a-zA-Z0-9_']*)\s*::\s*[^:\n]+->.*
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
