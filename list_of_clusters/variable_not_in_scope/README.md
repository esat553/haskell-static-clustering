# Variable nicht im Gültigkeitsbereich (Variable Not in Scope)

## Deutsche Beschreibung
Dieser Fehler tritt auf, wenn eine Variable verwendet wird, bevor sie definiert wurde oder außerhalb ihres Gültigkeitsbereichs liegt. Häufige Ursachen sind Tippfehler, falsche Groß-/Kleinschreibung, fehlende Funktionsparameter oder vergessene Definitionen im Codeblock.

## Mögliche Behebung (Fix)
Überprüfe Schreibweise, Position und Existenz der verwendeten Variable. Stelle sicher, dass sie korrekt deklariert und innerhalb des gültigen Kontexts verwendet wird. Achte insbesondere auf Namensgleichheit mit bereits verwendeten Bezeichnern und auf korrekte Parameterübergaben.

## English Description
This error occurs when a variable is used before it is defined or outside its valid scope. Common causes include typos, incorrect casing, missing parameters, or undefined variables within the code.

## Suggested Fix
Check the spelling, location, and existence of the variable. Ensure that it is properly declared and used within its valid context. Pay special attention to name consistency and correct parameter passing.


## Regex Pattern
```python
not in scope
```

For a complete student example including the original error message, see [example_1.hs](./example_1.hs).
