# Nicht definierter Datenkonstruktor (Data Constructor Not Defined)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn ein Datenkonstruktor verwendet wird, ohne dass er zuvor im Code definiert oder über ein importiertes Modul verfügbar gemacht wurde. In der Regel fehlt eine entsprechende `data`- oder `newtype`-Definition oder der Konstruktorname wurde falsch geschrieben.

## Mögliche Behebung (Fix)
Stelle sicher, dass alle verwendeten Konstruktoren durch eine gültige `data`- oder `newtype`-Definition im Code vorhanden sind oder durch ein korrektes `import` eingebunden wurden. Überprüfe auch die Schreibweise und beachte die Groß-/Kleinschreibung.

## English Description
This error occurs when a data constructor is used without being defined in the code or imported from a module. Typically, the corresponding `data` or `newtype` declaration is missing, or the constructor name is misspelled.

## Suggested Fix
Ensure that all used constructors are defined via a proper `data` or `newtype` declaration, or imported from the appropriate module. Also check for typos and case sensitivity in constructor names.

## Regex Pattern
```python
Not in scope: data constructor ‘[A-Z][a-zA-Z0-9_']*’
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
